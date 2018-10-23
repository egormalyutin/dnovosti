{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Args where

import qualified Database.MongoDB as M
import           System.Console.CmdArgs
import           Network.Socket

data Args = Serve {
                    port :: Int
                  , mongo_url :: HostName
                  , mongo_table :: M.Database
                  , data_path :: String
                  }
          | Reset {
                    mongo_url :: HostName
                  , mongo_table :: M.Database
                  , admin_name :: String
                  , admin_password :: String
                  , data_path :: String
                  }
            deriving (Show, Data, Typeable)

serveMode = Serve {
                    mongo_url   = "127.0.0.1" &= help "URL of MongoDB server"       &= typ "URL"  &= opt ("127.0.0.1" :: String)
                  , mongo_table = "dnovosti"  &= help "Name of MongoDB table"       &= typ "NAME" &= opt ("dnovosti" :: String)
                  , port        = 8080        &= help "Port to serve [8080]"        &= typ "PORT" &= opt (8080 :: Int)

                  , data_path = "./data" &= help "Path to save and serve data [./data]" &= typ "PATH" &= opt ("./data" :: String)
                  }
                  &= help "Serve Dnovosti"

resetMode = Reset {
                    mongo_url   = "127.0.0.1" &= help "URL of MongoDB server" &= typ "URL"  &= opt ("127.0.0.1" :: String)
                  , mongo_table = "dnovosti"  &= help "Name of MongoDB table" &= typ "NAME" &= opt ("dnovosti" :: String)

                  , admin_name = "admin" &= help "Admin username" &= typ "NAME" &= opt ("admin" :: String)
                  , admin_password = "1234" &= help "Admin password" &= typ "PASSWORD" &= opt ("1234" :: String)

                  , data_path = "./data" &= help "Path to save and serve data [./data]" &= typ "PATH" &= opt ("./data" :: String)
                  }
                  &= help "Reset and prepare MongoDB for Dnovosti, remove images from data dir"

mongoURL      = mongo_url
mongoTable    = mongo_table
dataPath      = data_path
adminName     = admin_name
adminPassword = admin_password

parseArgs :: IO Args
parseArgs = cmdArgs $ modes [serveMode, resetMode]
                      &= summary "Dnovosti server"
                      &= program "dnovosti"
