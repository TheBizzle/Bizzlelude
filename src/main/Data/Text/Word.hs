{-# LANGUAGE NoImplicitPrelude #-}
module Data.Text.Word(center, chunksOf, compareLength, count, drop, dropEnd, findIndex, index, justifyLeft, justifyRight, length, replicate, splitAt, take, takeEnd, unfoldrN) where

import External(($), Bool, Char, fromIntegral, Maybe, Ordering, Text, Word)
import Misc((&>), map)

import qualified Data.Text as T


center :: Word -> Char -> Text -> Text
center a b c = T.center (fromIntegral a) b c

chunksOf :: Word -> Text -> [Text]
chunksOf a b = T.chunksOf (fromIntegral a) b

compareLength :: Text -> Word -> Ordering
compareLength a b = T.compareLength a (fromIntegral b)

count :: Text -> Text -> Word
count a b = fromIntegral $ T.count a b

dropEnd :: Word -> Text -> Text
dropEnd a b = T.dropEnd (fromIntegral a) b

drop :: Word -> Text -> Text
drop a b = T.drop (fromIntegral a) b

findIndex :: (Char -> Bool) -> Text -> Maybe Word
findIndex f = (T.findIndex f) &> (map fromIntegral)

index :: Text -> Word -> Char
index a b = T.index a (fromIntegral b)

justifyLeft :: Word -> Char -> Text -> Text
justifyLeft a b c = T.justifyLeft (fromIntegral a) b c

justifyRight :: Word -> Char -> Text -> Text
justifyRight a b c = T.justifyRight (fromIntegral a) b c

length :: Text -> Word
length = T.length &> fromIntegral

replicate :: Word -> Text -> Text
replicate a b = T.replicate (fromIntegral a) b

splitAt :: Word -> Text -> (Text, Text)
splitAt a b = T.splitAt (fromIntegral a) b

takeEnd :: Word -> Text -> Text
takeEnd a b = T.takeEnd (fromIntegral a) b

take :: Word -> Text -> Text
take a b = T.take (fromIntegral a) b

unfoldrN :: Word -> (a -> Maybe (Char, a)) -> a -> Text
unfoldrN a f c = T.unfoldrN (fromIntegral a) f c
