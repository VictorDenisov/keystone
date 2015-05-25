{-# LANGUAGE OverloadedStrings #-}
module Auth
where
import Control.Applicative ((<*>), (<$>))
import Control.Monad (forM, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (verifyPassword)
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (Value(..), (.=), object, ToJSON(..))
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust, maybeToList)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Text.Read (readMaybe)

import qualified Common.Database as CD
import qualified Database.MongoDB as M
import qualified Model.User as MU
import qualified Model.Token as MT
import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Data.Text as T

data AuthRequest = AuthRequest
                 { methods  :: [AuthMethod]
                 , scope    :: Maybe AuthScope
                 } deriving Show

data AuthMethod = PasswordMethod
                { userId     :: M.ObjectId
                , password   :: String
                } deriving Show

data AuthScope = AuthScope
               { projectId :: M.ObjectId
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
          PasswordMethod <$> (userSpec .: "id") <*> (userSpec .: "password")
    scope <- runMaybeT $ do
      s <- MaybeT $ identity .:? "scope"
      p <- MaybeT $ s .:? "project"
      i <- MaybeT $ p .:? "id"
      return $ AuthScope i
    return $ AuthRequest ms scope

authenticate :: (MonadIO m)
             => (Maybe AuthScope) -> M.Pipe -> AuthMethod -> m (Either String (String, MT.Token))
authenticate mScope pipe (PasswordMethod userId password) = do
    mu <- CD.runDB pipe $ MU.findUserById userId
    scopeProjectId <- CD.runDB pipe $ calcProjectId mScope
    case mu of
      Nothing -> return $ Left "User is not found."
      Just u  ->
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
      (AuthScope pid) <- MaybeT $ return mScope
      project <- MaybeT $ MP.findProjectById pid
      return pid

authenticate _ _ _ = return $ Left "Method is not supported."

produceTokenResponse :: MonadIO m => MT.Token -> String -> M.Action m Value
produceTokenResponse (MT.Token issued expires user mProjectId) baseUrl = do
  u <- fromJust `liftM` MU.findUserById user
  scopeFields <- runMaybeT $ do
    pid <- MaybeT $ return mProjectId
    project <- MaybeT $ MP.findProjectById pid
    assignments <- lift $ MP.listAssignments (Just $ MP.ProjectId pid)
                                      (Just $ MU.UserId user)
    return $ [ "project" .= (object [ "id"   .= pid
                                   , "name" .= MP.name project
                                   , "links" .= (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show pid)) ])
                                   , "domain" .= ( object [ "name" .= ("Default" :: String)
                                                          , "id"   .= ("default" :: String)
                                                          ]
                                                 )
                                   ]
                           )
             , "roles" .= (object [])] -- TODO add roles to token response

  return $ object [ "token" .= ( object $ [ "expires_at" .= expires
                                        , "issued_at"  .= issued
                                        , "methods"    .= [ "password" :: String ]
                                        , "extras"    .= (object [])
                                        , "user"       .= (object [ "name"   .= MU.name u
                                                                  , "id"     .= (show user)
                                                                  , "domain" .= ( object [ "name" .= ("Default" :: String)
                                                                                         , "id"   .= ("default" :: String)])
                                                                  ] )
                                        ] ++ (concat $ maybeToList scopeFields))
                  ]
  where
    assignmentToRoleReply (MP.Assignment _ (MR.RoleId roleId) _ ) = runMaybeT $ do
      role <- MaybeT $ MR.findRoleById roleId
      return $ object [ "id" .= roleId
                      , "name" .= MR.name role
                      ]
