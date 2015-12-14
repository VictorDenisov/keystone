module Model.Ldap.User
where

import Control.Monad (forM_)
import Data.Bson (genObjectId)

import qualified Config as C
import qualified LDAP as L
import qualified Model.User as MU

listUsers :: C.LdapConfig -> L.LDAP -> IO [MU.User]
listUsers c l = do
  entries <- L.ldapSearch l (Just $ C.userTreeDn c) L.LdapScopeSubtree Nothing L.LDAPAllUserAttrs False
  putStrLn $ "-----------"
  forM_ entries $ \e -> do
    putStrLn $ "Name is - " ++ (L.ledn e)
    forM_ (L.leattrs e) $ \(attrName, attrValues) -> do
      putStrLn $ "  " ++ attrName ++ " " ++ (show attrValues)
  putStrLn $ "-----------"
  oid <- genObjectId
  return [MU.User oid (Just "Description") (Just "email") True "Simply Name" (Just "Password")]
