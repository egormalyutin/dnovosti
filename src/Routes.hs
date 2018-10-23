{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Control.Monad
import Control.Monad.IO.Class
import Happstack.Server hiding (serveFile)
import Database.MongoDB
import API
import Args
import Post

-- TODO: mutex new posts

routes :: Args -> Pipe -> IO (ServerPartT IO Response)
routes args@(Serve port url table path) mongo = do
    apiRouter <- api args mongo

    let srv file = srvFile $ path ++ "/" ++ file

    return $ msum [
                    nullDir >> srv "index.html"
                  , dir "newPost" $ srv "newPost.html"

                  , dir "static" $ srvDir $ path ++ "/static/"

                  , apiRouter
                  ]
