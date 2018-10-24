{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tuple3(mapFst3, mapSnd3, mapThd3, mapAll3, mapEach3, mapFstF3, mapSndF3, mapThdF3, tuple3To4a, tuple3To4b, tuple3To4c, tuple3To4d, curry3, uncurry3, fst3, snd3, thd3) where

import External

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

mapFst3 :: (a -> x) -> (a, b, c) -> (x, b, c)
mapFst3 f (a, b, c) = (f a, b, c)

mapSnd3 :: (b -> x) -> (a, b, c) -> (a, x, c)
mapSnd3 f (a, b, c) = (a, f b, c)

mapThd3 :: (c -> x) -> (a, b, c) -> (a, b, x)
mapThd3 f (a, b, c) = (a, b, f c)

mapAll3 :: (a -> x) -> (a, a, a) -> (x, x, x)
mapAll3 f (a, b, c) = (f a, f b, f c)

mapEach3 :: (a -> x) -> (b -> y) -> (c -> z) -> (a, b, c) -> (x, y, z)
mapEach3 f g h (a, b, c) = (f a, g b, h c)

mapFstF3 :: Functor f => (a -> f x) -> (a, b, c) -> f (x, b, c)
mapFstF3 f (a, b, c) = (f a) <&> (\x -> (x, b, c))

mapSndF3 :: Functor f => (b -> f x) -> (a, b, c) -> f (a, x, c)
mapSndF3 f (a, b, c) = (f b) <&> (\x -> (a, x, c))

mapThdF3 :: Functor f => (c -> f x) -> (a, b, c) -> f (a, b, x)
mapThdF3 f (a, b, c) = (f c) <&> (\x -> (a, b, x))

tuple3To4a :: (a, b, c) -> x -> (x, a, b, c)
tuple3To4a (a, b, c) x = (x, a, b, c)

tuple3To4b :: (a, b, c) -> x -> (a, x, b, c)
tuple3To4b (a, b, c) x = (a, x, b, c)

tuple3To4c :: (a, b, c) -> x -> (a, b, x, c)
tuple3To4c (a, b, c) x = (a, b, x, c)

tuple3To4d :: (a, b, c) -> x -> (a, b, c, x)
tuple3To4d (a, b, c) x = (a, b, c, x)

curry3 :: ((a, b, c) -> x) -> a -> b -> c -> x
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> x) -> ((a, b, c) -> x)
uncurry3 f (a, b, c) = f a b c

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
