{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Start (start) where

import           Control.Monad
import           Happstack.Server
import qualified Database.MongoDB as M
import           Post
import           Reset
import qualified Args             as A
import           Routes

-- TODO: mutex new posts

serve :: A.Args -> IO ()
serve args = do
    let port = A.port args

    putStrLn $ "Listening on http://localhost:" ++ show port

    mongo <- M.connect . M.host $ A.mongoURL args

    router <- routes args mongo

    simpleHTTP nullConf { port = port } $ router

reset :: A.Args -> IO ()
reset args = do
    putStrLn "Resetting..."

    mongo <- M.connect $ M.host (A.mongoURL args)
    M.access mongo M.master (A.mongoTable args) resetDB

    putStrLn "Resetted!"

start :: IO ()
start = do
    args <- A.parseArgs

    case args of
        A.Serve{..} -> serve args
        A.Reset{..}   -> reset args
