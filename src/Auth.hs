{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Auth
where
import Common (loggerName)
import Control.Applicative ((<*>), (<$>))
import Control.Monad (forM, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (verifyPassword)
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (Value(..), (.=), object, ToJSON(..))
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust, maybeToList, catMaybes, listToMaybe)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Vector (fromList)
import System.Log.Logger (debugM)
import Text.Read (readMaybe)

import qualified Common.Database as CD
import qualified Database.MongoDB as M
import qualified Model.User as MU
import qualified Model.Token as MT
import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.Service as MS
import qualified Data.Text as T

data AuthRequest = AuthRequest
                 { methods :: [AuthMethod]
                 , scope   :: Maybe AuthScope
                 } deriving Show

data AuthMethod = PasswordMethod
                { userId   :: Maybe M.ObjectId
                , userName :: Maybe String
                , domainId :: Maybe String
                , password :: String
                } deriving Show

data AuthScope = ProjectIdScope
               { projectId     :: Maybe M.ObjectId
               , projectName   :: Maybe String
               , scopeDomainId :: Maybe String
               } deriving Show

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
            MaybeT $ domainSpec .:? "id"
          PasswordMethod <$> (userSpec .:? "id") <*> (userSpec .:? "name") <*> (return mDomainId) <*> (userSpec .: "password")
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

authenticate :: (MonadBaseControl IO m, MonadIO m)
             => (Maybe AuthScope) -> M.Pipe -> AuthMethod -> m (Either String (String, MT.Token))
authenticate mScope pipe (PasswordMethod mUserId mUserName mDomainId password) = do
    mu <- case mUserId of
              Just userId ->
                runMaybeT $ do
                  mu <- MaybeT $ CD.runDB pipe $ MU.findUserById userId
                  return (userId, mu)
              Nothing -> do
                users <- CD.runDB pipe $ MU.listUsers mUserName
                return $ listToMaybe users
    scopeProjectId <- CD.runDB pipe $ calcProjectId mScope
    case mu of
      Nothing -> return $ Left "User is not found."
      Just (userId, u)  ->
        case MU.password u of
          Just p ->
            if verifyPassword (pack password) (pack p)
              then do
                currentTime <- liftIO getCurrentTime
                let token = MT.Token currentTime (addUTCTime (fromInteger $ 8 * 60 * 60) currentTime) userId scopeProjectId
                mt <- CD.runDB pipe $ MT.createToken token
                return $ Right (show mt, token)
              else return $ Left "Passwords don't match."
          Nothing -> return $ Left "User exists, but doesn't have any password."
  where
    calcProjectId mScope = runMaybeT $ do
      (ProjectIdScope mPid mPname _) <- MaybeT $ return mScope
      case mPid of
        Just pid -> do
          project <- MaybeT $ MP.findProjectById pid
          return pid
        Nothing -> do
          (pid, _) <- MaybeT $ listToMaybe <$> MP.listProjects mPname
          return pid

authenticate _ _ _ = return $ Left "Method is not supported."

produceTokenResponse :: (MonadBaseControl IO m, MonadIO m) => MT.Token -> String -> M.Action m Value
produceTokenResponse (MT.Token issued expires user mProjectId) baseUrl = do
  liftIO $ debugM loggerName $ "Producing token response"
  u <- fromJust `liftM` MU.findUserById user
  scopeFields <- runMaybeT $ do
    pid <- MaybeT $ return mProjectId
    liftIO $ debugM loggerName $ "pid" ++ (show pid)
    project <- MaybeT $ MP.findProjectById pid
    liftIO $ debugM loggerName $ "project" ++ (show project)
    assignments <- lift $ MP.listAssignments (Just $ MP.ProjectId pid)
                                             (Just $ MU.UserId user)
    liftIO $ debugM loggerName $ "Received assignments " ++ (show assignments)
    mroles <- lift $ mapM assignmentToRoleReply assignments
    liftIO $ debugM loggerName $ "Received roles " ++ (show mroles)
    let roles = catMaybes mroles
    return $ [ "project" .= (object [ "id"   .= pid
                                   , "name" .= MP.name project
                                   , "links" .= (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show pid)) ])
                                   , "domain" .= ( object [ "name" .= ("Default" :: String)
                                                          , "id"   .= ("default" :: String)
                                                          ]
                                                 )
                                   ]
                           )
             , "roles" .= (Array $ fromList roles)
             ]
  endpoints <- MS.listServices
  liftIO $ debugM loggerName $ "Scope fields are: " ++ (show scopeFields)

  return $ object [ "token" .= ( object $ [ "expires_at" .= expires
                                          , "issued_at"  .= issued
                                          , "methods"    .= [ "password" :: String ]
                                          , "extras"     .= (object [])
                                          , "user"       .= (object [ "name"   .= MU.name u
                                                                    , "id"     .= (show user)
                                                                    , "domain" .= ( object [ "name" .= ("Default" :: String)
                                                                                         , "id"   .= ("default" :: String)])
                                                                  ] )
                                          , "catalog"  .= (Array $ fromList $ map serviceToValue endpoints)
                                        ] ++ (concat $ maybeToList scopeFields))
                  ]
  where
    assignmentToRoleReply :: MonadIO m => MP.Assignment -> M.Action m (Maybe Value)
    assignmentToRoleReply (MP.Assignment _ (MR.RoleId roleId) _) = runMaybeT $ do
      role <- MaybeT $ MR.findRoleById roleId
      return $ object [ "id" .= roleId
                      , "name" .= MR.name role
                      ]

    serviceToValue :: (M.ObjectId, MS.Service) -> Value
    serviceToValue (serviceId, service) =
               object [ "id"        .= serviceId
                      , "name"      .= MS.name service
                      , "type"      .= MS.type' service
                      , "endpoints" .= (map endpointToValue $ MS.endpoints service)
                      ]
    endpointToValue :: MS.Endpoint -> Value
    endpointToValue endpoint = object
                             [ "id"        .= MS.eid endpoint
                             , "interface" .= MS.einterface endpoint
                             , "region"    .= (String "Default")
                             , "region_id" .= (String "default")
                             , "url"       .= MS.eurl endpoint
                             ]
