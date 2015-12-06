{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Auth
where
import Web.Auth.Types
import Backend (BackendApi(..))
import Common (loggerName)
import Config (KeystoneConfig(..))
import Control.Applicative ((<*>), (<$>))
import Control.Exception (throwIO)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (verifyPassword)
import Data.Aeson (FromJSON(..), (.:), (.:?), eitherDecode')
import Data.Aeson.Types ( Value(..), (.=), object, typeMismatch
                        , Object)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (readFile)
import Data.Maybe (maybeToList, listToMaybe)
import Data.IORef (IORef(..), readIORef, newIORef, writeIORef)
import Data.List (find, (\\))
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)
import Network.HTTP.Types.Status (status401)
import Prelude hiding (readFile)
import System.Log.Logger (noticeM)
import Text.Read (readMaybe)
import Web.Common (ActionM)

import qualified Common.Database as CD
import qualified Error as E
import qualified Database.MongoDB as M
import qualified Model.Assignment as MA
import qualified Model.Domain as MD
import qualified Model.User as MU
import qualified Model.Token as MT
import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.Service as MS
import qualified Web.Scotty.Trans as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy as LT
import qualified Data.HashMap.Strict as HM

authenticate :: (MonadIO m, MonadIO (b m), BackendApi (b m), Functor (b m))
             => M.Pipe
             -> (Maybe AuthScope)
             -> AuthMethod
             -> b m (Either String (String, MT.Token))
authenticate pipe mScope (PasswordMethod mUserId mUserName mDomainId mDomainName password) = do
  res <- checkUserPassword mUserId mUserName password
  case res of
    Left  errorMessage -> return $ Left errorMessage
    Right user -> do
      liftIO $ Right <$> (CD.runDB pipe $ produceToken mScope user)

produceToken :: (Maybe AuthScope) -> MU.User -> M.Action IO (String, MT.Token)
produceToken  mScope user = do
      currentTime  <- liftIO $ getCurrentTime
      scopeProject <- calcProject mScope
      scopeRoles   <- calcRoles scopeProject user
      services     <- MS.listServices Nothing
      tokenId      <- liftIO $ M.genObjectId
      let token = MT.Token
                        tokenId
                        currentTime
                        (addUTCTime (fromInteger $ 8 * 60 * 60) currentTime)
                        user
                        scopeProject
                        scopeRoles
                        services
      mt <- MT.createToken token
      return (show mt, token)
  where
    calcProject mScope = runMaybeT $ do
      (ProjectIdScope mPid mPname _) <- MaybeT $ return mScope
      MaybeT $ case mPid of
        Just pid -> MP.findProjectById pid
        Nothing  -> listToMaybe <$> MP.listProjects mPname

    calcRoles mProject user = do
      list <- runMaybeT $ do
        project <- MaybeT $ return mProject
        lift $ MA.listUserRoles (MP.ProjectId $ MP._id project) (MU.UserId $ MU._id user)
      return $ concat $ maybeToList list

--authenticate _ _ _ = return $ Left "Method is not supported."

authorize :: (MonadIO (b IO), BackendApi (b IO))
          => (HM.HashMap Action Verifier)
          -> Action
          -> MT.Token
          -> Resource
          -> ActionM (b IO) ()
          -> ActionM (b IO) ()
authorize verifiers action token resource actionToRun =
  if (verifiers HM.! action) resource token
  then actionToRun
  else do
    S.status status401
    S.json $ E.unauthorized "You are not authorized to perform this action"

requireToken :: (MonadIO (b IO), BackendApi (b IO))
            => KeystoneConfig
            -> (MT.Token -> ActionM (b IO) ())
            -> ActionM (b IO) ()
requireToken config actionToRun = do
  let adminToken = Config.adminToken config
  req <- S.request
  mToken <- S.header hXAuthToken
  case mToken of
    Nothing -> do
      S.status status401
      S.json $ E.unauthorized "Token is required"
    Just m ->
      if m == (LT.pack adminToken)
        then
          actionToRun MT.AdminToken
        else do
          let mTokenId = readMaybe $ LT.unpack m
          case mTokenId of
            Nothing -> do
              S.status status401
              S.json $ E.unauthorized "Wrong token"
            Just tokenId  -> do
              mToken <- liftIO $ CD.withDB (database config) $ MT.findTokenById tokenId
              case mToken of
                Nothing -> do
                  S.status status401
                  S.json $ E.unauthorized "Wrong token"
                Just token -> actionToRun token

loadPolicy :: IO Policy
loadPolicy = do
  liftIO $ noticeM loggerName "Loading policy"
  content <- readFile "policy.json"
  let res = (eitherDecode' content :: Either String Value)
  case  res of
    Left errorString -> throwIO $ PolicyCompileException $ "Failed to parse policy json: " ++ errorString
    Right jsonPolicy -> compilePolicy jsonPolicy

compilePolicy :: Value -> IO (HM.HashMap Action Verifier)
compilePolicy (Object policy) =
  case HM.lookup identityF policy of
    Nothing -> throwIO $ PolicyCompileException "Expected key 'identity' in policy file"
    Just vActionRules -> withObject (T.unpack identityF) vActionRules $ \actionRules -> do
      let rulesOnly = HM.delete identityF policy
      ruleMap <- HM.traverseWithKey convertRule $ rulesOnly

      resList <- mapM (compileActionRule ruleMap) $ HM.toList actionRules

      let policyActions = map fst resList
      case ( policyActions \\ listOfImplementedActions
           , listOfImplementedActions \\ policyActions) of
        ([], []) -> return $ HM.fromList resList
        ([], missingActions) -> throwIO $ PolicyCompileException $ "Actions missing from the policy " ++ (show missingActions)
        (unknownActions, missingActions) -> throwIO $ PolicyCompileException $ "Unknown actions " ++ (show unknownActions)
  where
    convertRule :: T.Text -> Value -> IO (IORef (Either Value Verifier))
    convertRule _ v = newIORef (Left v)

    compileActionRule :: HM.HashMap T.Text (IORef (Either Value Verifier))
                      -> (T.Text, Value)
                      -> IO (Action, Verifier)
    compileActionRule verifiers (actionName, expression) = do
      case readMaybe $ T.unpack actionName of
        Nothing     -> throwIO $ PolicyCompileException $ "Unknown action " ++ (T.unpack actionName)
        Just action -> do
          verifier <- guardAdminToken <$> compileExpression verifiers expression
          return $ (action, verifier)

    withObject :: String -> Value -> (Object -> IO a) -> IO a
    withObject keyName (Object v) f = f v
    withObject keyName _ _ = throwIO $ PolicyCompileException $ "Expected object at the key " ++ keyName

    identityF :: T.Text
    identityF = "identity"

compilePolicy _ = throwIO $ PolicyCompileException "Root value in policy file should be Object"

compileExpression :: HM.HashMap T.Text (IORef (Either Value Verifier)) -> Value -> IO Verifier
compileExpression verifiers (Object m) = do
  case (head $ HM.toList m) of
    ("or", (Array operands)) -> do
      vs <- V.mapM (compileExpression verifiers) operands
      return $ \resource token -> V.or $ V.map (\v -> v resource token) vs
    ("or", _) -> throwIO $ PolicyCompileException "or expression value should be array"
    ("and", (Array operands)) -> do
      vs <- V.mapM (compileExpression verifiers) operands
      return $ \resource token -> V.and $ V.map (\v -> v resource token) vs
    ("and", _) -> throwIO $ PolicyCompileException "and expression value should be array"
    ("not", operand) -> do
      v <- compileExpression verifiers operand
      return $ \resource token -> not $ v resource token
    ("rule", (String "owner")) -> do
      return $ \resource token -> isOwner resource token
    ("rule", (String ruleName)) -> do
      let mRef = ruleName `HM.lookup` verifiers
      case mRef of
        Nothing  -> throwIO $ PolicyCompileException $ "rule " ++ (T.unpack ruleName) ++ " is not defined"
        Just ref -> do
          expr <- readIORef ref
          case expr of
            Left s -> do
              verifier <- compileExpression verifiers s
              writeIORef ref $ Right verifier
              return verifier
            Right v -> return v
    ("rule", _) -> throwIO $ PolicyCompileException "rule expression value should be string"
    ("role", (String r)) -> do
      let role = T.unpack r
      return $ \resource token -> maybe
                           False
                           (const True)
                           $ find
                               (\tokenRole -> role == (MR.name tokenRole))
                               (MT.roles token)
    ("role", _) -> throwIO $ PolicyCompileException "role expression value should be string"
    (expr, _) -> throwIO $ PolicyCompileException $ "Unknown expression: " ++ (T.unpack expr)
compileExpression verifiers expr = throwIO $ PolicyCompileException $ "Error while compiling policy expression " ++ (show expr) ++ ". Expecting object instead"


checkUserPassword :: ( MonadIO m
                     , BackendApi (b m)
                     , MonadIO (b m)
                     ) => Maybe M.ObjectId
                       -> Maybe String
                       -> String
                       -> b m (Either String MU.User)
checkUserPassword mUserId mUserName passwordToCheck = do
  mu <- case mUserId of
            Just userId -> findUserById userId
            Nothing -> do
              users <- listUsers mUserName
              return $ listToMaybe users
  case mu of
    Nothing -> return $ Left "User is not found."
    Just user  ->
      case MU.password user of
        Just p ->
          if verifyPassword (pack passwordToCheck) (pack p)
            then return $ Right user
            else return $ Left "The password if incorrect"
        Nothing -> return $ Left "User exists, but doesn't have any password."

instance FromJSON AuthRequest where
  parseJSON (Object v) = do
    auth <- v .: "auth"
    identity <- auth .: "identity"
    mNames <- identity .: "methods"
    ms <- forM mNames $ \m -> do
      mDescr <- identity .: m
      case m of
        "password" -> do
          userSpec <- mDescr .: "user"
          mDomainId <- runMaybeT $ do
            domainSpec <- MaybeT $ userSpec .:? "domain"
            idString   <- MaybeT $ domainSpec .:? "id"
            MaybeT $ return $ readMaybe idString
          mDomainName <- runMaybeT $ do
            domainSpec <- MaybeT $ userSpec .:? "domain"
            MaybeT $ domainSpec .:? "name"
          PasswordMethod <$> (userSpec .:? "id")
                         <*> (userSpec .:? "name")
                         <*> (return mDomainId)
                         <*> (return mDomainName)
                         <*> (userSpec .: "password")
    scope <- runMaybeT $ do
      s <- MaybeT $ auth .:? "scope"
      p <- MaybeT $ s .:? "project"
      i <- lift $ p .:? "id"
      n <- lift $ p .:? "name"
      di <- lift $ runMaybeT $ do
        d <- MaybeT $ p .:? "domain"
        MaybeT $ d .:? "id"
      return $ ProjectIdScope i n di
    return $ AuthRequest ms scope
  parseJSON v = typeMismatch (nameBase ''AuthRequest) v

produceTokenResponse :: MT.Token -> String -> Value
produceTokenResponse (MT.Token _ issued expires user mProject roles services) baseUrl =
  object [ "token" .= (object $ [ "expires_at" .= expires
                                , "issued_at"  .= issued
                                , "methods"    .= [ "password" :: String ]
                                , "extras"     .= (object [])
                                , "user"       .= (object [ "name"   .= MU.name user
                                                          , "id"     .= (show $ MU._id user)
                                                          , "domain" .= ( object [ "name" .= MD.defaultDomainName
                                                                                 , "id"   .= MD.defaultDomainId])
                                                          ] )
                                , "catalog"  .= (Array $ fromList $ map serviceToValue services)
                                ] ++ (concat $ maybeToList scopeFields))
         ]
  where
    scopeFields = do
      project <- mProject
      let projectValue = calcProjectScope project baseUrl
      return $ [ "project" .= projectValue
               , "roles"   .= (Array $ fromList $ map toRoleReply roles)
               ]

    toRoleReply :: MR.Role -> Value
    toRoleReply (MR.Role roleId roleName _ _)
                      = object [ "id"   .= roleId
                               , "name" .= roleName
                               ]

    serviceToValue :: MS.Service -> Value
    serviceToValue service
             = object [ "id"        .= MS._id service
                      , "name"      .= MS.name service
                      , "type"      .= MS.type' service
                      , "endpoints" .= (map endpointToValue $ MS.endpoints service)
                      ]
    endpointToValue :: MS.Endpoint -> Value
    endpointToValue endpoint = object
                             [ "id"        .= MS.eid endpoint
                             , "interface" .= MS.einterface endpoint
                             , "region"    .= MD.defaultDomainName
                             , "region_id" .= (String $ T.pack MD.defaultDomainId)
                             , "url"       .= MS.eurl endpoint
                             ]

calcProjectScope :: MP.Project -> String -> Value
calcProjectScope project baseUrl
          = object [ "id"     .= MP._id project
                   , "name"   .= MP.name project
                   , "links"  .= (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show $ MP._id project)) ])
                   , "domain" .= (object [ "name" .= MD.defaultDomainName
                                         , "id"   .= MD.defaultDomainId
                                         ])
                   ]

listOfImplementedActions :: [Action]
listOfImplementedActions = enumFrom $ toEnum 0

isOwner :: Resource -> MT.Token -> Bool
isOwner (Token tokenToVerify) token = (MU._id $ MT.user token) == (MU._id $ MT.user tokenToVerify)
isOwner (UserId userId) token = (MU._id $ MT.user token) == userId
isOwner (EmptyResource) _ = True

hXAuthToken :: LT.Text
hXAuthToken = "X-Auth-Token"

guardAdminToken :: Verifier -> Verifier
guardAdminToken v r MT.AdminToken = True
guardAdminToken v r t = v r t
