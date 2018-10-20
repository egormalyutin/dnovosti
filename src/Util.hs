{-# LANGUAGE OverloadedStrings #-}

module Util where

import Database.MongoDB

infixr 1 <$>>
a <$>> b = b <$> a

type ID = Int

-- Get ID for new *something*
getNewID :: String -> Action IO ID
getNewID for = do
    doc <- findAndModifyOpts (select ["type" =: for] "counters")
                             FamUpdate {
                               famUpdate = ["$inc" =: ["seq" =: (1 :: Int)]]
                             , famNew = True
                             , famUpsert = True
                             }

    case doc of
        Right (Just counter) -> return $ at "seq" counter
        otherwise -> return 0

-- Add item to collection
addItem :: Collection -> (a -> Document) -> a -> Action IO ()
addItem collection processor item = do
    insert collection $ processor item
    return ()

-- Get items by ID
getItems :: Collection -> Label -> (Document -> a) -> [ID] -> Action IO [a]
getItems collection key processor id = do
    doc <- rest =<< find (select [key =: ["$in" =: id]] collection)
    return $ processor <$> doc
