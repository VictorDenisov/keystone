module Common
where

maybeNothing :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
maybeNothing v f = maybe (return Nothing) f v

databaseVersion :: String
databaseVersion = "0.0.1"

loggerName :: String
loggerName = "Main"
