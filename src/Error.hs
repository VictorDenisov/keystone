{-# LANGUAGE OverloadedStrings #-}
module Error
where

import Data.Aeson (ToJSON(..), object, (.=))

data Error = Error
  { code    :: Int
  , message :: String
  , title   :: String
  }

instance ToJSON Error where
  toJSON (Error code message title) = object
    [ "error" .= (object
        [ "code"    .= code
        , "message" .= message
        , "title"   .= title
        ] )
    ]

unauthorized :: String -> Error
unauthorized message = Error 401 message "Not Authorized"

notFound :: String -> Error
notFound message = Error 404 message "Not Found"
