module Model.Ldap.User
where

import Data.Maybe (listToMaybe, catMaybes, fromMaybe)

import qualified Config as C
import qualified Database.MongoDB as M
import qualified LDAP as L
import qualified Model.User as MU

listUsers :: Maybe String -> C.LdapConfig -> L.LDAP -> IO [MU.User]
listUsers mUserName c l = do
  entries <- listUserEntries mUserName c l
  let users = catMaybes $ map (entryToUser c) (filter (isObjectClass $ C.userObjectClass c) entries)
  return users

listUserEntries :: Maybe String -> C.LdapConfig -> L.LDAP -> IO [L.LDAPEntry]
listUserEntries mUserName c l = do
  let userNameFilter = do
        userName <- mUserName
        return $ (C.userNameAttribute c) ++ "=" ++ userName
  entries <- L.ldapSearch l (Just $ C.userTreeDn c) L.LdapScopeSubtree userNameFilter L.LDAPAllUserAttrs False
  return entries

findUserById :: C.LdapConfig -> L.LDAP -> M.ObjectId -> IO (Maybe MU.User)
findUserById c l oid = do
  e <- findUserEntryById c l oid
  return $ e >>= (entryToUser c)

findUserEntryById :: C.LdapConfig -> L.LDAP -> M.ObjectId -> IO (Maybe L.LDAPEntry)
findUserEntryById c l oid = do
  entries <- L.ldapSearch l (Just $ C.userTreeDn c) L.LdapScopeSubtree (Just $ "employeeNumber=" ++ (show oid)) L.LDAPAllUserAttrs False
  return $ listToMaybe entries

checkUserPassword :: C.LdapConfig -> L.LDAP -> Maybe M.ObjectId -> Maybe String -> String -> IO (Either String MU.User)
checkUserPassword c l mUserId mUserName passwordToCheck = do
  mu <- case mUserId of
            Just userId -> findUserEntryById c l userId
            Nothing -> do
              users <- listUserEntries mUserName c l
              return $ listToMaybe users
  case mu of
    Nothing   -> return $ Left "User is not found."
    Just userEntry -> do
      let dn = L.ledn userEntry
      user <- L.catchLDAP
        (do
          L.ldapSimpleBind l dn passwordToCheck
          let mu = entryToUser c userEntry
          case mu of
            Nothing -> return $ Left "Failed to parse user from entry"
            Just u -> return $ Right u
        )
        $ \(L.LDAPException code description  caller) -> return $ Left "Authentication failed"
      L.ldapSimpleBind l "cn=admin,dc=test,dc=com" "qwerty"
      return user

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
