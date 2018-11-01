{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Image (uploadImageWorker, getImageWithSize, ImageWithSize) where

import           Codec.Picture
import           Codec.Picture.Extra
import           Files
import           Args
import           Database.MongoDB
import qualified Database.MongoDB as DB
import           Data.ByteString hiding (filter)
import qualified Data.ByteString.Lazy as L
import           Util
import           Control.Concurrent
import           Data.Aeson
import           GHC.Generics

data ImageWithSize = ImageWithSize {
                                     width :: Int
                                   , height :: Int
                                   , filename :: String
                                   } deriving (Generic)

instance ToJSON ImageWithSize

sizes :: [Int]
sizes = [2000, 1000, 500, 300]

updateImage :: Int -> Int -> Int -> Bool -> ByteString -> Action IO ()
updateImage id width height original bs = do
    name <- uploadFile bs
    findAndModifyOpts (select ["imageID" =: id] "images")
                      FamUpdate {
                        famUpdate = if original
                            then ["$push" =: ["images" =: ["width" =: width, "height" =: height, "filename" =: name, "original" =: True]]]
                            else ["$push" =: ["images" =: ["width" =: width, "height" =: height, "filename" =: name]]]
                      , famNew = True
                      , famUpsert = True
                      }
    return ()

uploadImageWorker :: ByteString -> Pipe -> Args -> ID -> IO (Either String Int)
uploadImageWorker bs mongo args post =
    case decodeImage bs of
        Left err -> return $ Left err
        Right image -> do
            id <- access mongo master (mongoTable args) $ getNewImageID

            forkIO $ do
                access mongo master (mongoTable args) $ insert "images" ["imageID" =: id, "images" =: ([] :: [Document])]

                let img = convertRGBA8 image
                    width = imageWidth img
                    k = (fromIntegral $ imageHeight img) / (fromIntegral width)
                    ns = filter (\size -> width >= size) sizes

                access mongo master (mongoTable args) $ updateImage id (imageWidth img) (imageHeight img) False bs

                access mongo master (mongoTable args) $ sequence $
                    ((\newWidth ->
                        let newHeight = ceiling $ k * (fromIntegral newWidth)
                        in  updateImage id newWidth newHeight False . L.toStrict . encodePng . scaleBilinear newWidth newHeight $ img
                    ) <$> ns)

                return ()

            return $ Right id

-- Get ImageWithSize with image ID
getImageWithSize :: ID -> Action IO [ImageWithSize]
getImageWithSize id = do
    f <- findOne $ select ["imageID" =: id] "images"
    dt <- DB.lookup "images" $ extractMaybe f
    return $ dt <$>> \d -> ImageWithSize (at "width" d) (at "height" d) (at "filename" d)

-- Get ID for new image
getNewImageID :: Action IO ID
getNewImageID = getNewID "images"

extractMaybe :: Maybe a -> a
extractMaybe (Just x) = x

-- Old stupid code :p

-- pipeCmd cmd args = CreateProcess { cmdspec = RawCommand cmd args
--                                  , cwd = Nothing
--                                  , env = Nothing
--                                  , std_in = CreatePipe
--                                  , std_out = CreatePipe
--                                  , std_err = Inherit
--                                  , close_fds = False
--                                  , create_group = False
--                                  , delegate_ctlc = False
--                                  , detach_console = False
--                                  , create_new_console = False
--                                  , new_session = False
--                                  , child_group = Nothing
--                                  , child_user = Nothing
--                                  , use_process_jobs = False
--                                  }

-- resize :: ByteString -> Int -> IO (Maybe ByteString)
-- resize bs newWidth = do
--     (Just hin, Just hout, _, process) <- createProcess $ pipeCmd "convert" ["-", "-resize", show newWidth, "-"]
--     hSetBinaryMode hin  True
--     hSetBinaryMode hout True
--     hPut hin bs
--     waitForProcess process
--     res <- hGetContents hout
--     putStrLn "hsdusha"
--     return $ Just res

-- uploadSizes :: ByteString -> [Int] -> Action IO (Maybe [String])
-- uploadSizes _ [] = return $ Just []
-- uploadSizes bs (w:xs) = do
--     resized <- lift $ resize bs w
--     case resized of
--         Just im -> do
--             other <- uploadSizes bs xs
--             case other of
--                 Just other -> do
--                     name <- uploadFile im
--                     return . Just $ name : other
--                 Nothing -> return Nothing
--         Nothing -> return Nothing

-- uploadImage :: ByteString -> Action IO (Maybe [String])
-- uploadImage bs = uploadSizes bs sizes


-- uploadImage :: ByteString -> Action IO (Maybe [String])
-- uploadImage bs =
--     case decodeImage bs of
--         Left _ -> return Nothing
--         Right image -> do
--             let img = convertRGBA8 image
--                 width = imageWidth img
--                 k = (fromIntegral $ imageHeight img) / (fromIntegral width)
--                 ns = filter (\size -> width >= size) sizes

--             names <- mapM uploadFile $ L.toStrict . encodePng <$>
--                 ((\newWidth ->
--                     scaleBilinear newWidth (ceiling $ k * (fromIntegral newWidth)) img
--                 ) <$> ns)
--             return $ Just names
