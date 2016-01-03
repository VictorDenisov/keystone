{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Token
where

import Model.Mongo.Common (idF)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Bson ((=:), ObjectId)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.Time.Clock (getCurrentTime, UTCTime)

import Database.MongoDB.Internal.Util ((<.>))

import Language.Haskell.TH.Syntax (nameBase)

import qualified Data.Text as T
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA
import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.Service as MS
import qualified Keystone.Model.User as MU

collectionName :: M.Collection
collectionName = "token"

data Token = Token { _id       :: M.ObjectId
                   , issuedAt  :: UTCTime
                   , expiresAt :: UTCTime
                   , user      :: MU.User
                   , project   :: Maybe MP.Project -- currently only project scope is available
                   , roles     :: [MR.Role]
                   , services  :: [MS.Service]
                   }
           | AdminToken
             deriving (Show, Eq, Typeable)

$(deriveBson id ''Token)

createToken :: Token -> M.Action IO M.Value
createToken t =
  M.insert collectionName $ toBson t

findTokenById :: ObjectId -> M.Action IO (Maybe Token)
findTokenById tid = runMaybeT $ do
  mToken <- MaybeT $ M.findOne (M.select [idF =: tid] collectionName)
  fromBson mToken

validateToken :: ObjectId -> M.Action IO Bool
validateToken tid = do
  currentTime <- liftIO getCurrentTime
  res <- runMaybeT $ do
    token <- MaybeT $ M.findOne
                          $ (M.select [idF =: tid] collectionName)
                {M.project = [ (T.pack $ nameBase 'expiresAt) =: (M.Int32 1) ]}
    (M.UTC time) <- MaybeT $ return $ M.look
                                          (T.pack $ nameBase 'expiresAt)
                                          token
    if currentTime > time
      then return False
      else return True
  case res of
    Just v  -> return v
    Nothing -> return False

idxDocument :: M.Index -> M.Database -> M.Document
idxDocument M.Index{..} db = [
	"ns" =: db <.> iColl,
	"key" =: iKey,
	"name" =: iName,
	"unique" =: iUnique,
	"dropDups" =: iDropDups ]

listTokens :: M.Action IO [Token]
listTokens = do
  cursor <- M.find $ M.select [] collectionName
  docs <- M.rest cursor
  mapM fromBson docs

verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                        collectionName
                        [(T.pack $ nameBase 'expiresAt) =: (M.Int32 1)])
                   {MA.iExpireAfterSeconds = Just 0}
  tokens <- listTokens
  return ()
