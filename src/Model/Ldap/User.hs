module Model.Ldap.User
where

import Data.Maybe (listToMaybe, catMaybes, fromMaybe)

import qualified Config as C
import qualified LDAP as L
import qualified Model.User as MU

listUsers :: C.LdapConfig -> L.LDAP -> IO [MU.User]
listUsers c l = do
  entries <- L.ldapSearch l (Just $ C.userTreeDn c) L.LdapScopeSubtree Nothing L.LDAPAllUserAttrs False
  let users = catMaybes $ map (entryToUser c) (filter (isObjectClass $ C.userObjectClass c) entries)
  return users

isObjectClass :: String -> L.LDAPEntry -> Bool
isObjectClass oc e =
  let mValues = lookup "objectClass" (L.leattrs e) in
  case mValues of
    Nothing     -> False
    Just values -> elem oc values

entryToUser :: C.LdapConfig -> L.LDAPEntry -> Maybe MU.User
entryToUser c e = do
  name <- getUserAttribute (C.userNameAttribute c) (L.leattrs e)
  soid  <- getUserAttribute (C.userIdAttribute c) (L.leattrs e)
  return (MU.User
              (read soid)
              (Just "Description")
              (getUserAttribute (C.userMailAttribute c) (L.leattrs e))
              True
              (fromMaybe "" $ getUserAttribute (C.userNameAttribute c) (L.leattrs e))
              (getUserAttribute (C.userPassAttribute c) (L.leattrs e))
         )

getUserAttribute :: String -> [(String, [String])] -> Maybe String
getUserAttribute attribute leattrs = do
  attrs <- lookup attribute leattrs
  listToMaybe attrs
