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
import qualified Model.Assignment as MA
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
              Just userId -> CD.runDB pipe $ MU.findUserById userId
              Nothing -> do
                users <- CD.runDB pipe $ MU.listUsers mUserName
                return $ listToMaybe users
    case mu of
      Nothing -> return $ Left "User is not found."
      Just user  ->
        case MU.password user of
          Just p ->
            if verifyPassword (pack password) (pack p)
              then do
                currentTime  <- liftIO getCurrentTime
                scopeProject <- CD.runDB pipe $ calcProject mScope
                scopeRoles   <- CD.runDB pipe $ calcRoles scopeProject user
                services     <- CD.runDB pipe $ MS.listServices
                let token = MT.Token
                                  currentTime
                                  (addUTCTime (fromInteger $ 8 * 60 * 60) currentTime)
                                  user
                                  scopeProject
                                  scopeRoles
                                  services
                mt <- CD.runDB pipe $ MT.createToken token
                return $ Right (show mt, token)
              else return $ Left "Passwords don't match."
          Nothing -> return $ Left "User exists, but doesn't have any password."
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


authenticate _ _ _ = return $ Left "Method is not supported."


calcProjectScope :: MP.Project -> String -> Value
calcProjectScope project baseUrl
          = object [ "id"     .= MP._id project
                   , "name"   .= MP.name project
                   , "links"  .= (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show $ MP._id project)) ])
                   , "domain" .= (object [ "name" .= ("Default" :: String)
                                         , "id"   .= ("default" :: String)
                                         ])
                   ]

produceTokenResponse :: MT.Token -> String -> Value
produceTokenResponse (MT.Token issued expires user mProject roles services) baseUrl =
  object [ "token" .= (object $ [ "expires_at" .= expires
                                , "issued_at"  .= issued
                                , "methods"    .= [ "password" :: String ]
                                , "extras"     .= (object [])
                                , "user"       .= (object [ "name"   .= MU.name user
                                                          , "id"     .= (show $ MU._id user)
                                                          , "domain" .= ( object [ "name" .= ("Default" :: String)
                                                                                 , "id"   .= ("default" :: String)])
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
                             , "region"    .= (String "Default")
                             , "region_id" .= (String "default")
                             , "url"       .= MS.eurl endpoint
                             ]
