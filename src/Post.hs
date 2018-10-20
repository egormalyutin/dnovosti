{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Post where

import Database.MongoDB
import GHC.Generics
import Data.Aeson
import Util

-- Content of Post
data Content = Text  {     text :: String }
             | Image { filename :: String }
               deriving (Generic, Show)

instance ToJSON Content

-- Post on site
data Post = Post {
                   postID :: ID
                 , name :: String
                 , content :: [Content]
                 } deriving (Generic, Show)

instance ToJSON Post

-- Convert Post to BSON
toMongoPost :: Post -> Document
toMongoPost post = [
                     "postID" =: postID post
                   , "name" =: name post
                   , "content" =: content post <$>>
                        \x -> case x of

                            -- Convert Content to BSON
                            Text str       -> [ "type" =: ("text"  :: String), "text"     =: str      ]
                            Image filename -> [ "type" =: ("image" :: String), "filename" =: filename ]
                   ]

-- Convert BSON to Post
toPost :: Document -> Post
toPost doc = Post {
                    postID = at "postID" doc
                  , name = at "name" doc
                  , content = at "content" doc <$>>
                        \doc -> case at "type" doc :: String of

                        -- Convert BSON to Content
                            "text"  -> Text  $ at "text"     doc
                            "image" -> Image $ at "filename" doc
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
