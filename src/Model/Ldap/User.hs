module Model.Ldap.User
where

import Control.Monad (forM_)
import Data.Bson (genObjectId)

import qualified LDAP as L
import qualified Model.User as MU

listUsers :: String -> L.LDAP -> IO [MU.User]
listUsers userTreeDn l = do
  entries <- L.ldapSearch l (Just userTreeDn) L.LdapScopeSubtree Nothing L.LDAPAllUserAttrs False
  putStrLn $ "-----------"
  forM_ entries $ \e -> do
    putStrLn $ "Name is - " ++ (L.ledn e)
    forM_ (L.leattrs e) $ \(attrName, attrValues) -> do
      putStrLn $ "  " ++ attrName ++ " " ++ (show attrValues)
  putStrLn $ "-----------"
  oid <- genObjectId
  return [MU.User oid (Just "Description") (Just "email") True "Simply Name" (Just "Password")]
