{-# LANGUAGE OverloadedStrings #-}

module Reset where

import Database.MongoDB
import System.Directory

resetDB :: Action IO ()
resetDB = do
    dropCollection "posts"
    dropCollection "files"
    dropCollection "counters"

    createCollection [] "posts"
    createCollection [] "files"
    createCollection [] "counters"

    insert "counters" ["type" =: ("posts" :: String), "seq" =: (-1 :: Int)]
    insert "counters" ["type" =: ("files" :: String), "seq" =: (-1 :: Int)]

    return ()
