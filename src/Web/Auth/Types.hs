{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Web.Auth.Types where

import Control.Exception (Exception(..))
import Data.Hashable (Hashable)
import Data.Data (Typeable)
import GHC.Generics (Generic)

import qualified Database.MongoDB as M
import qualified Data.HashMap.Strict as HM
import qualified Model.Token as MT

data AuthRequest = AuthRequest
                 { methods :: [AuthMethod]
                 , scope   :: Maybe AuthScope
                 } deriving Show

data AuthMethod = PasswordMethod
                { userId   :: Maybe M.ObjectId
                , userName :: Maybe String
                , domainId :: Maybe M.ObjectId
                , domainName :: Maybe String
                , password :: String
                } deriving Show

data AuthScope = ProjectIdScope
               { projectId     :: Maybe M.ObjectId
               , projectName   :: Maybe String
               , scopeDomainId :: Maybe String
               } deriving Show

data Action = ValidateToken
            | CheckToken
            -- | RevokeToken

            | AddService
            | ListServices
            | ShowServiceDetails
            | UpdateService
            | DeleteService

            | AddEndpoint
            | ListEndpoints
            | ShowEndpoint
            -- | UpdateEndpoint
            | DeleteEndpoint

            -- | AddDomain
            | ListDomains
            | ShowDomainDetails
            -- | UpdateDomain
            -- | DeleteDomain
            -- | ListRolesForDomainUser
            -- | GrantRoleToDomainUser
            -- | CheckRoleForDomainUser
            -- | RevokeRoleForDomainUser
            -- | ListRolesForDomainGroup
            -- | GrantRoleToDomainGroup
            -- | CheckRoleForDomainGroup
            -- | RevokeRoleForDomainGroup

            | AddProject
            | ListProjects
            | ShowProjectDetails
            -- | UpdateProject
            | DeleteProject
            | ListRolesForProjectUser
            | GrantRoleToProjectUser
            -- | CheckRoleForProjectUser
            -- | RevokeRoleForProjectUser
            -- | ListRolesForProjectGroup
            -- | GrantRoleToProjectGroup
            -- | CheckRoleForProjectGroup
            -- | RevokeRoleForProjectGroup

            | AddUser
            | ListUsers
            | ShowUserDetails
            | UpdateUser
            | DeleteUser
            -- | ListGroupsForUser
            | ListProjectsForUser
            | ChangePassword

            -- | AddGroup
            -- | ListGroups
            -- | ShowGroupDetails
            -- | UpdateGroup
            -- | DeleteGroup
            -- | ListUsersInGroup
            -- | AddUserToGroup
            -- | RemoveUserFromGroup
            -- | CheckUserMembershipInGroup

            -- | AddCredential
            -- | ListCredentials
            -- | ShowCredentialDetails
            -- | UpdateCredential
            -- | DeleteCredential

            | AddRole
            | ListRoles
            | ShowRoleDetails
            | ListRoleAssignments
            | DeleteRole

            -- | AddPolicy
            -- | ListPolicies
            -- | ShowPolicyDetails
            -- | UpdatePolicy
            -- | DeletePolicy
              deriving (Show, Read, Eq, Generic, Enum)

instance Hashable Action

data Resource = Token MT.Token
              | UserId M.ObjectId
              | EmptyResource

type Policy = HM.HashMap Action Verifier

type Verifier = Resource -> MT.Token -> Bool

data PolicyCompileException = PolicyCompileException String
                              deriving (Typeable, Show)

instance Exception PolicyCompileException

