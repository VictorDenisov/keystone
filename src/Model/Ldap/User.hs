module Model.Ldap.User
where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either (rights)

import qualified Config as C
import qualified Database.MongoDB as M
import qualified LDAP as L
import qualified Model.User as MU

listUsers :: Maybe String -> C.LdapConfig -> L.LDAP -> IO [MU.User]
listUsers mUserName c l = do
  entries <- listUserEntries mUserName c l
  let eUsers = map (entryToUser c) (filter (isObjectClass $ C.userObjectClass c) entries)
  return $ rights eUsers -- log rights

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
  case e of
    Nothing -> return Nothing
    Just v  -> do
      case entryToUser c v of
        Left err -> return Nothing -- TODO log err
        Right u -> return $ Just u

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
          let eu = entryToUser c userEntry
          case eu of
            Left e -> return $ Left $ "Failed to parse user from entry: " ++ e
            Right u -> return $ Right u
        )
        $ \(L.LDAPException code description  caller) -> return $ Left "Authentication failed"
      let userName = C.userDn c
      let password = C.password c
      L.ldapSimpleBind l userName password
      return user

isObjectClass :: String -> L.LDAPEntry -> Bool
isObjectClass oc e =
  let mValues = lookup "objectClass" (L.leattrs e) in
  case mValues of
    Nothing     -> False
    Just values -> elem oc values

entryToUser :: C.LdapConfig -> L.LDAPEntry -> Either String MU.User
entryToUser c e = do
  name <- getUserAttributeE (C.userNameAttribute c) (L.leattrs e)
  soid  <- getUserAttributeE (C.userIdAttribute c) (L.leattrs e)
  return (MU.User
              (read soid)
              (Just "Description")
              (getUserAttributeM (C.userMailAttribute c) (L.leattrs e))
              True
              (fromMaybe "" $ getUserAttributeM (C.userNameAttribute c) (L.leattrs e))
              (getUserAttributeM (C.userPassAttribute c) (L.leattrs e))
         )

getUserAttributeM :: String -> [(String, [String])] -> Maybe String
getUserAttributeM attribute leattrs = do
  attrs <- lookup attribute leattrs
  listToMaybe attrs

getUserAttributeE :: String -> [(String, [String])] -> Either String String
getUserAttributeE attribute leattrs =
  maybe (Left $ "Missing attribute " ++ attribute) Right $ do
    attrs <- lookup attribute leattrs
    listToMaybe attrs
