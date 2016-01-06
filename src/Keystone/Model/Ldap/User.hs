module Keystone.Model.Ldap.User
where

import Common (loggerName)
import Control.Monad (forM_)

import Data.Maybe (listToMaybe)
import Data.Either (lefts, rights)

import System.Log.Logger (warningM)

import Text.Read (readMaybe)

import qualified Config as C
import qualified Database.MongoDB as M
import qualified LDAP as L
import qualified Keystone.Model.User as MU

listUsers :: Maybe String -> C.LdapConfig -> L.LDAP -> IO [MU.User]
listUsers mUserName c l = do
  entries <- listUserEntries mUserName c l
  let eUsers = map (entryToUser c) (filter (isObjectClass $ C.userObjectClass c) entries)

  forM_ (lefts eUsers) $ \(err, entry) -> do
    warningM loggerName $ "Failed to interpret ldap user. Error: " ++ err ++ ". User: " ++ (show entry)
  return $ rights eUsers

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
        Left (err, entry) -> do
          warningM loggerName $ "Failed to interpret ldap user. Error: " ++ err ++ ". User: " ++ (show entry)
          return Nothing
        Right u -> return $ Just u

findUserEntryById :: C.LdapConfig -> L.LDAP -> M.ObjectId -> IO (Maybe L.LDAPEntry)
findUserEntryById c l oid = do
  entries <- L.ldapSearch l (Just $ C.userTreeDn c) L.LdapScopeSubtree (Just $ (C.userIdAttribute c) ++ "=" ++ (show oid)) L.LDAPAllUserAttrs False
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
            Left e -> return $ Left $ "Failed to parse user from entry: " ++ (show e)
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

entryToUser :: C.LdapConfig -> L.LDAPEntry -> Either (String, L.LDAPEntry) MU.User
entryToUser c e = do
  name <- getUserAttributeE (C.userNameAttribute c) e
  oid  <- readUserId c e
  return (MU.User
              oid
              (Just "Description")
              (getUserAttributeM (C.userMailAttribute c) e)
              True
              name
              (getUserAttributeM (C.userPassAttribute c) e)
         )

readUserId :: C.LdapConfig -> L.LDAPEntry -> Either (String, L.LDAPEntry) M.ObjectId
readUserId c e =
  case getUserAttributeE (C.userIdAttribute c) e of
    Left err  -> Left err
    Right uid ->
      case readMaybe uid of
        Nothing -> Left ("Failed to parse user id from attribute " ++ (C.userIdAttribute c), e)
        Just v  -> Right v

getUserAttributeM :: String -> L.LDAPEntry -> Maybe String
getUserAttributeM attribute (L.LDAPEntry _ leattrs) = do
  attrs <- lookup attribute leattrs
  listToMaybe attrs

getUserAttributeE :: String -> L.LDAPEntry -> Either (String, L.LDAPEntry) String
getUserAttributeE attribute entry =
  maybe
    (Left ("Missing attribute " ++ attribute, entry))
    Right
    (getUserAttributeM attribute entry)
