{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tuple2(mapFst, mapSnd, mapBoth, mapFstF, mapSndF, tuple2To3a, tuple2To3b, tuple2To3c) where

import External

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

mapFst :: (a -> x) -> (a, b) -> (x, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> x) -> (a, b) -> (a, x)
mapSnd f (a, b) = (a, f b)

mapBoth :: (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth f g (a, b) = (f a, g b)

mapFstF :: Functor f => (a -> f x) -> (a, b) -> f (x, b)
mapFstF f (a, b) = (f a) <&> (\x -> (x, b))

mapSndF :: Functor f => (b -> f x) -> (a, b) -> f (a, x)
mapSndF f (a, b) = (f b) <&> (\x -> (a, x))

tuple2To3a :: (a, b) -> x -> (x, a, b)
tuple2To3a (a, b) x = (x, a, b)

tuple2To3b :: (a, b) -> x -> (a, x, b)
tuple2To3b (a, b) x = (a, x, b)

tuple2To3c :: (a, b) -> x -> (a, b, x)
tuple2To3c (a, b) x = (a, b, x)
