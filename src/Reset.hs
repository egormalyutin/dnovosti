{-# LANGUAGE OverloadedStrings #-}

module Reset where

import Args
import Database.MongoDB hiding (addUser)
import System.Directory
import User

resetDB :: Args -> Action IO ()
resetDB args = do
    dropCollection "posts"
    dropCollection "users"
    dropCollection "files"
    dropCollection "images"
    dropCollection "counters"

    createCollection [] "posts"
    createCollection [] "users"
    createCollection [] "files"
    createCollection [] "images"
    createCollection [] "counters"

    insert "counters" ["type" =:  ("posts" :: String), "seq" =: (-1 :: Int)]
    insert "counters" ["type" =:  ("users" :: String), "seq" =: (-1 :: Int)]
    insert "counters" ["type" =:  ("files" :: String), "seq" =: (-1 :: Int)]
    insert "counters" ["type" =: ("images" :: String), "seq" =: (-1 :: Int)]

    adminID <- getNewUserID
    addUser $ User {
                     userID = adminID
                   , login = adminName args
                   , password = adminPassword args
                   , permissions = allPerms
                   }

    return ()
