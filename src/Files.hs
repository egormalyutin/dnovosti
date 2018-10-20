{-# LANGUAGE OverloadedStrings #-}

module Files (uploadFile, getFile) where

import           Args
import qualified Data.ByteString  as BS
import           Database.MongoDB
import           Util
import qualified Data.List        as L

------------------------------------------------
-------------------- UPLOAD --------------------
------------------------------------------------

-- Upload chunks to MongoDB
uploadChunks :: [BS.ByteString] -> String -> Action IO ()
uploadChunks chunks name = do
    id <- getNewFileID
    let docs = toDocs chunks name
    insertMany "files" docs
    return ()

-- Upload file to MongoDB
uploadFile :: Pipe -> Args -> BS.ByteString -> IO String
uploadFile mongo args bs = do
    let chunks = splitBy (10 ^ 6) bs
    id <- access mongo master (mongoTable args) $ getNewFileID
    let name = show id
    access mongo master (mongoTable args) $ uploadChunks chunks name
    return name

---------------------------------------------
-------------------- GET --------------------
---------------------------------------------

-- Download chunks from MongoDB
getChunks :: String -> Action IO [BS.ByteString]
getChunks name = do
    docs <- rest =<< find (select ["name" =: name] "files")
    let docs' = L.sortBy (\x y -> compare ((at "index" x) :: Int) ((at "index" y) :: Int)) docs
    return $ toChunks docs'

-- Download file from MongoDB
getFile :: Pipe -> Args -> String -> IO (Maybe BS.ByteString)
getFile mongo args name = do
    chunks <- access mongo master (mongoTable args) $ getChunks name
    if length chunks > 0
        then return . Just $ BS.concat chunks
        else return Nothing

------------------------------------------------------
-------------------- TRANSFORMERS --------------------
------------------------------------------------------

-- Transform chunks to documents
toDocs :: [BS.ByteString] -> String -> [Document]
toDocs chunks name = imap chunks $ \i c -> [
                                             "name" =: name
                                           , "index" =: i
                                           , "chunk" =: Binary c
                                           ]

-- Transform documents to chunks
toChunks :: [Document] -> [BS.ByteString]
toChunks = map $ extractBS . at "chunk"

-------------------------------------------------
-------------------- HELPERS --------------------
-------------------------------------------------

-- Extract ByteString from Binary
extractBS :: Binary -> BS.ByteString
extractBS (Binary bs) = bs

-- Get ID for new file
getNewFileID :: Action IO ID
getNewFileID = getNewID "files"

-- Map with indexes
imap :: [a] -> (Int -> a -> b) -> [b]
imap a f = m 0 f a
    where m _ _ [] = []
          m i f (x:xs) = f i x : m (i+1) f xs

-- Split ByteString to chunks of ByteString by number of bytes
splitBy :: Int -> BS.ByteString -> [BS.ByteString]
splitBy n bs = if bs /= BS.empty
                   then BS.take n bs : splitBy n (BS.drop n bs)
                   else []
