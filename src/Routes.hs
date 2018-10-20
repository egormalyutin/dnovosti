{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Control.Monad
import Control.Monad.IO.Class
import Happstack.Server
import Database.MongoDB
import API
import Args
import Post

-- TODO: mutex new posts

routes :: Args -> Pipe -> IO (ServerPartT IO Response)
routes args@(Serve port url table path) mongo = do
    apiRouter <- api args mongo
    return $ msum [
                    serveDirectory EnableBrowsing ["index.html"] path
                  , apiRouter
                  ]
