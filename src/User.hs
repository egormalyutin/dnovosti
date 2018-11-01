{-# LANGUAGE OverloadedStrings #-}

module User where

import Database.MongoDB hiding (find)
import Util
import Data.List (find)
import Control.Monad.Trans

-- User permisssions
data Permission = CreatePosts | RegisterUsers deriving (Show, Eq)

-- Mappings from Permission to String
permMappings :: [(Permission, String)]
permMappings = [(CreatePosts, "createPosts"), (RegisterUsers, "registerUsers")]

-- All permissions
allPerms :: [Permission]
allPerms = fst <$> permMappings

-- Convert String to Permission
toPerm :: String -> Permission
toPerm str = case find (\(_,s) -> s == str) permMappings of
    Just (t,_) -> t
    Nothing -> error $ "No permission mapping for " ++ str

-- Convert Permission to String
toMongoPerm :: Permission -> String
toMongoPerm tp = case find (\(t,_) -> t == tp) permMappings of
    Just (_,p) -> p
    Nothing -> error $ "No permission mapping for " ++ show tp

-- Post on site
data User = User {
                   userID :: ID
                 , login :: String
                 , password :: String
                 , permissions :: [Permission]
                 }

-- Convert User to BSON
toMongoUser :: User -> Document
toMongoUser user = [
                     "userID" =: userID user
                   , "login" =: login user
                   , "password" =: password user
                   , "permissions" =: permissions user <$>> toMongoPerm
                   ]

-- Convert BSON to User
toUser :: Document -> User
toUser doc = User {
                    userID = at "userID" doc
                  , login = at "login" doc
                  , password = at "password" doc
                  , permissions = at "permissions" doc <$>> toPerm
                  }

-- Get users by IDs
getUsers :: [ID] -> Action IO [User]
getUsers = getItems "users" "userID" toUser

-- Get ID for new user
getNewUserID :: Action IO ID
getNewUserID = getNewID "users"

-- Add user to users
addUser :: User -> Action IO ()
addUser = addItem "users" toMongoUser

-- Check user login, password and permissions
checkAuth :: String -> String -> [Permission] -> Action IO Bool
checkAuth login password perms = do
    user <- findOne $ select ["login" =: login, "password" =: password] "users"
    case user of
        Just doc -> do
            let user = toUser doc
            return $ checkPerms (permissions user) perms

        Nothing -> return False

-- Check user permissions and needed
checkPerms :: [Permission] -> [Permission] -> Bool
checkPerms userPerms = foldr (\perm all -> all && Nothing /= (find (==perm) userPerms)) True
