{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module API where

import           Control.Monad
import           Control.Monad.IO.Class
import           Args
import           Post
import           Data.Aeson
import           Database.MongoDB        hiding (look, filter)
import           Happstack.Server
import qualified Happstack.Server        as H (path)
import           System.Directory
import           Files
import qualified Happstack.Server.RqData as R
import           Data.ByteString         as BS hiding (filter)
import           Data.List.Split
import           Data.Maybe
import           GHC.Generics
import           Util
import           Happstack.Server.FileServe.BuildingBlocks

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
                        method POST

                        decodeBody bodyPolicy

                        -- extract post data
                        contents <- getAttachments mongo args
                        postName <- R.getDataFn $ R.look "name"

                        case postName of
                            Left _ -> badRequest $ respStr "Invalid request: no \"name\" field"

                            Right postName -> do
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

                  , dir "file" . H.path $ \name -> do
                        method GET

                        file <- liftIO $ getFile mongo args name
                        case file of
                            Just bs -> ok $ toResponse bs
                            Nothing -> notFound . respStr $ "Not found file " ++ name

                  , dir "getPosts" $ do
                        method GET

                        parseField "ids" parseIDs $ \nums -> do
                            posts <- liftIO . access mongo master table $ getPosts nums
                            ok . toResponse $ encode posts

                  , dir "getFeed" $ do
                        method GET

                        parseField "offset" parseInt $ \offset -> do
                            parseField "count" parseInt $ \count -> do
                                posts <- liftIO . access mongo master table $ getFeed offset count
                                ok . toResponse $ encode posts
                  ]


-------------------------------------------------
-------------------- HELPERS --------------------
-------------------------------------------------

-- Parser helper
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

----- PARSERS -----

parseIDs :: String -> Maybe [ID]
parseIDs str = toIDs $ splitOn "," str
               where toIDs :: [String] -> Maybe [Int]
                     toIDs [] = Just []
                     toIDs (x:xs) = case ((maybeRead x) :: Maybe Int) of
                         Just num -> let other = toIDs xs
                                     in case other of
                                         Nothing  -> Nothing
                                         Just arr -> Just $ num : arr
                         Nothing -> Nothing

parseInt :: String -> Maybe Int
parseInt = maybeRead

-- Error JSON response
newtype ErrResp = ErrResp { error :: String } deriving (Generic)
instance ToJSON ErrResp

-- Generate and send error JSON response
sendErr code = code . toResponse . encode . ErrResp

-- Generate string response
respStr :: String -> Response
respStr = toResponse

-- Check and parse field
parseField :: String -> (String -> Maybe a) -> (a -> ServerPartT IO Response) -> ServerPartT IO Response
parseField name parse succ = do
    field <- R.getDataFn $ R.look name
    case field of
        Left _ -> sendErr badRequest $ "Invalid request: No \"" ++ name ++ "\" field"
        Right str -> if "" /= filter (\l -> l /= ' ' && l /= '\t') str
            then do
                case parse str of
                    Just result -> succ result
                    Nothing -> sendErr badRequest $ "Invalid request: cannot parse \"" ++ name ++ "\" field"
            else sendErr badRequest $ "Invalid request: \"" ++ name ++ "\" field is empty"

-- File serving

-- Get mime type of file by path
getMimeType :: (Monad m) => FilePath -> m String
getMimeType path = do
    tp <- guessContentTypeM mimeTypes path
    return $ tp ++ "; charset=utf-8"

-- Serve file
srvFile :: String -> ServerPartT IO Response
srvFile path = serveFile getMimeType $ path

-- Serve directory by name
srvDir :: String -> ServerPartT IO Response
srvDir path = serveDirectory' DisableBrowsing [] getMimeType $ path
