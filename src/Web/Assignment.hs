{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Assignment
where

import Data.Aeson (Value(..))
import Data.Aeson.Types (object, (.=))
import Data.Vector (fromList)
import Model.Assignment (Assignment(..))

import Web.Common (UrlBasedValue, UrlInfo(..))


import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.User as MU

produceAssignmentJson :: Assignment -> String -> Value
produceAssignmentJson
          (Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid))
          baseUrl
      = object [ "role" .= (object ["id" .= rid])
               , "user" .= (object ["id" .= uid])
               , "scope" .= (object ["project" .= (object ["id" .= pid] )])
               , "links" .= (object ["assignment" .= (baseUrl ++ "/v3/projects/" ++ (show pid) ++ "/users/" ++ (show uid) ++ "/roles/" ++ (show rid))])
               ]

produceAssignmentsReply :: [Assignment] -> UrlBasedValue
produceAssignmentsReply assignments (UrlInfo {baseUrl, path, query})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ path ++ query)
                                  ]
                          )
             , "role_assignments" .= assignmentsEntry
             ]
  where
    assignmentsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\a -> produceAssignmentJson a) assignments

