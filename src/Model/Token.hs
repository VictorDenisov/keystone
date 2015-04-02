module Token
where

collectionName :: M.Collection
collectionName = "token"

data Token = Token { expiresAt :: Date
                   , issuedAt  :: Date
                   , user      :: ObjectId
                   }
