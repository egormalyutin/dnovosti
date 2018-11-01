{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Post where

import Database.MongoDB
import GHC.Generics
import Data.Aeson
import Util
import Image

-- Content of Post
data Content = Text  { text    :: String }
             | Image { imageID :: Int    }

             | ImageFull { filenames :: [ImageWithSize] }

               deriving (Generic)

instance ToJSON Content

-- Post on site
data Post = Post {
                   postID :: ID
                 , name :: String
                 , content :: [Content]
                 } deriving (Generic)

instance ToJSON Post

-- Convert Post to BSON
toMongoPost :: Post -> Document
toMongoPost post = [
                     "postID" =: postID post
                   , "name" =: name post
                   , "content" =: content post <$>>
                        \x -> case x of

                            -- Convert Content to BSON
                            Text str -> [ "type" =: ("text"  :: String), "text"    =: str ]
                            Image id -> [ "type" =: ("image" :: String), "imageID" =: id  ]
                   ]

-- Convert BSON to Post
toPost :: Document -> Post
toPost doc = Post {
                    postID = at "postID" doc
                  , name = at "name" doc
                  , content = at "content" doc <$>>
                        \doc -> case at "type" doc :: String of

                        -- Convert BSON to Content
                            "text"  -> Text  $ at "text"    doc
                            "image" -> Image $ at "imageID" doc
                  }

-- Get posts by IDs
getPosts :: [ID] -> Action IO [Post]
getPosts = getItems "posts" "postID" toPost

-- Get ID for new post
getNewPostID :: Action IO ID
getNewPostID = getNewID "posts"

-- Add post to posts
addPost :: Post -> Action IO ()
addPost = addItem "posts" toMongoPost

-- Generate post feed
getFeed :: Int -> Int -> Action IO [Post]
getFeed offset count = do
    docs <- rest =<< find (select [] "posts") {
                                                sort = ["postID" =: (-1 :: Int)]
                                              , skip = fromIntegral offset
                                              , limit = fromIntegral count
                                              }
    return $ toPost <$> docs

-- Load filenames in post
loadFilenames :: Post -> Action IO Post
loadFilenames post = do
    c <- mapM (
        \c -> case c of
            Image id -> do
                f <- getImageWithSize id
                return . ImageFull $ f

            other -> return other
        ) $ content post

    return $ post { content = c }
