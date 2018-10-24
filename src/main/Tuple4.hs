{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tuple4(mapFst4, mapSnd4, mapThd4, mapFrt4, mapAll4, mapEach4, mapFstF4, mapSndF4, mapThdF4, mapFrtF4, tuple4To5a, tuple4To5b, tuple4To5c, tuple4To5d, tuple4To5e, curry4, uncurry4, fst4, snd4, thd4, frt4) where

import External

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

mapFst4 :: (a -> x) -> (a, b, c, d) -> (x, b, c, d)
mapFst4 f (a, b, c, d) = (f a, b, c, d)

mapSnd4 :: (b -> x) -> (a, b, c, d) -> (a, x, c, d)
mapSnd4 f (a, b, c, d) = (a, f b, c, d)

mapThd4 :: (c -> x) -> (a, b, c, d) -> (a, b, x, d)
mapThd4 f (a, b, c, d) = (a, b, f c, d)

mapFrt4 :: (d -> x) -> (a, b, c, d) -> (a, b, c, x)
mapFrt4 f (a, b, c, d) = (a, b, c, f d)

mapAll4 :: (a -> x) -> (a, a, a, a) -> (x, x, x, x)
mapAll4 f (a, b, c, d) = (f a, f b, f c, f d)

mapEach4 :: (a -> x) -> (b -> y) -> (c -> z) -> (d -> l) -> (a, b, c, d) -> (x, y, z, l)
mapEach4 f g h i (a, b, c, d) = (f a, g b, h c, i d)

mapFstF4 :: Functor f => (a -> f x) -> (a, b, c, d) -> f (x, b, c, d)
mapFstF4 f (a, b, c, d) = (f a) <&> (\x -> (x, b, c, d))

mapSndF4 :: Functor f => (b -> f x) -> (a, b, c, d) -> f (a, x, c, d)
mapSndF4 f (a, b, c, d) = (f b) <&> (\x -> (a, x, c, d))

mapThdF4 :: Functor f => (c -> f x) -> (a, b, c, d) -> f (a, b, x, d)
mapThdF4 f (a, b, c, d) = (f c) <&> (\x -> (a, b, x, d))

mapFrtF4 :: Functor f => (d -> f x) -> (a, b, c, d) -> f (a, b, c, x)
mapFrtF4 f (a, b, c, d) = (f d) <&> (\x -> (a, b, c, x))

tuple4To5a :: (a, b, c, d) -> x -> (x, a, b, c, d)
tuple4To5a (a, b, c, d) x = (x, a, b, c, d)

tuple4To5b :: (a, b, c, d) -> x -> (a, x, b, c, d)
tuple4To5b (a, b, c, d) x = (a, x, b, c, d)

tuple4To5c :: (a, b, c, d) -> x -> (a, b, x, c, d)
tuple4To5c (a, b, c, d) x = (a, b, x, c, d)

tuple4To5d :: (a, b, c, d) -> x -> (a, b, c, x, d)
tuple4To5d (a, b, c, d) x = (a, b, c, x, d)

tuple4To5e :: (a, b, c, d) -> x -> (a, b, c, d, x)
tuple4To5e (a, b, c, d) x = (a, b, c, d, x)

curry4 :: ((a, b, c, d) -> x) -> a -> b -> c -> d -> x
curry4 f a b c d = f (a, b, c, d)

uncurry4 :: (a -> b -> c -> d -> x) -> ((a, b, c, d) -> x)
uncurry4 f (a, b, c, d) = f a b c d

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

frt4 :: (a, b, c, d) -> d
frt4 (_, _, _, d) = d
