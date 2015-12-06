{-# LANGUAGE FlexibleInstances #-}
module Web.Common
where

import Data.Aeson (Value(..))
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Web.Scotty.Internal.Types (ActionT(..))

import qualified Error as E

import qualified Web.Scotty.Trans as S

type ScottyM m = S.ScottyT E.Error m
type ActionM m = ActionT E.Error m

instance MonadThrow m => MonadThrow (ActionT E.Error m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (ActionT E.Error m) where
  catch (ActionT m) c = ActionT $ catch m (runAM . c)

data UrlInfo = UrlInfo
             { baseUrl :: String
             , path    :: String
             , query   :: String
             }

type UrlBasedValue = UrlInfo -> Value

