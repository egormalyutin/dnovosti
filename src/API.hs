{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad
import           Control.Monad.IO.Class
import           Args
import           Post
import           Database.MongoDB        hiding (look)
import           Happstack.Server
import qualified Happstack.Server        as S (path)
import           System.Directory
import           Files
import qualified Happstack.Server.RqData as R
import           Data.ByteString         as BS

bodyPolicy :: BodyPolicy
bodyPolicy = defaultBodyPolicy "/tmp/" (30*10^6) (30*10^6) 100000

getAttachments :: Pipe -> Args -> ServerPartT IO [Content]
getAttachments mongo args = getter 0
    where getter i = do
              r1 <- R.getDataFn . R.look $ "text" ++ (show i)
              case r1 of
                  Right text -> do
                      other <- getter $ i + 1
                      return $ Text text : other
                  Left _ -> do
                      r2 <- R.getDataFn . R.lookFile $ "image" ++ (show i)
                      case r2 of
                          Right (path, _, _) -> do
                              other <- getter $ i + 1

                              -- upload file contents to MongoDB
                              contents <- liftIO $ BS.readFile path
                              name <- liftIO $ uploadFile mongo args contents

                              return $ Image name : other
                          Left _ -> return []

api :: Args -> Pipe -> IO (ServerPartT IO Response)
api args@(Serve port url table path) mongo = do
    let publishedPath = path ++ "/published.html"
    return $ msum [
                    dir "publish" $ do
                        decodeBody bodyPolicy

                        -- extract post data
                        contents <- getAttachments mongo args
                        postName <- look "name"
                        postID <- liftIO . access mongo master table $ getNewPostID

                        -- push post
                        let post = Post {
                                          postID = postID
                                        , name = postName
                                        , content = contents
                                        }

                        liftIO . access mongo master table $ addPost post

                        -- serve published.html
                        serveFile (guessContentTypeM mimeTypes) publishedPath

                  , dir "file" . S.path $ \name -> do
                        file <- liftIO $ getFile mongo args name
                        case file of
                            Just bs -> ok $ toResponse bs
                            Nothing -> notFound $ toResponse (("Not found file " ++ name) :: String)
                  ]
