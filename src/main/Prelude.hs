module Prelude((|>), (<&>), asPath, asString, asText, cartProduct, concat, curry3, curry4, error, frt4, fst3, fst4, groupOn, listDirsRecursively, map, mapAllFour, mapAllThree, mapBoth, mapFrt4, mapFrtF4, mapFst, mapFst3, mapFst4, mapFstF, mapFstF3, mapFstF4, mapSnd, mapSnd3, mapSnd4, mapSndF, mapSndF3, mapSndF4, mapThd3, mapThd4, mapThdF3, mapThdF4, pam, putStrFlush, regexMatch, return', scalaGroupBy, showText, snd3, snd4, thd3, thd4, tuple2To3a, tuple2To3b, tuple2To3c, tuple3To4a, tuple3To4b, tuple3To4c, tuple3To4d, tuple4To5a, tuple4To5b, tuple4To5c, tuple4To5d, tuple4To5e, uncurry3, uncurry4, uncurry5, unsafeRead
  , module Control.Arrow, module Control.Applicative, module Control.Monad, module Control.Monad.IO.Class, module Data.Bifunctor, module Data.Bool, module Data.Char, module Data.Either, module Data.Eq, module Data.Foldable, module Data.Function, module Data.Functor, module Data.Int, module Data.IntSet, module Data.Map, module Data.Maybe, module Data.Monoid, module Data.Ord, module Data.Semigroup, module Data.Set, module Data.Text, module Data.Tuple, module Debug.Trace, module GHC.Base, module GHC.Err, module GHC.Float, module GHC.IO, module GHC.Num, module GHC.Real, module GHC.Show, module Numeric, module System.IO.Error, module Text.Read) where

import Control.Arrow((&&&), (***), (>>>))
import Control.Applicative(Alternative((<|>)), Applicative((<*>), (<*), (*>), pure))
import Control.Monad((>=>), filterM, foldM, foldM_, forM, forM_, guard, join, mapM, mapM_, Monad((>>), (>>=), return), MonadPlus(), sequence, sequence_, unless, when)
import Control.Monad.IO.Class(liftIO)

import Data.Bifunctor(Bifunctor(bimap, first, second))
import Data.Bool(Bool(False, True), (&&), (||), not, otherwise)
import Data.Char(Char, digitToInt, intToDigit)
import Data.Either(Either(Left, Right), either, lefts, isLeft, isRight, partitionEithers, rights)
import Data.Eq(Eq((==), (/=)))
import Data.Foldable(Foldable(fold, foldMap, foldr, foldr', foldl, foldl', foldr1, foldl1, null, length, elem, maximum, minimum, sum, product), foldlM, for_, sequenceA_, and, or, any, all, maximumBy, minimumBy, find)
import Data.Function(($), (.), const, flip, id, on)
import Data.Functor((<$), (<$>), ($>), Functor(fmap), void)
import Data.Int(Int, Int8, Int16, Int32, Int64)
import Data.IntSet(IntSet)
import Data.Map(Map)
import Data.Maybe(catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybe, maybeToList, Maybe(Just, Nothing))
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord((<), (<=), (>), (>=), compare, max, min), Ordering(EQ, GT, LT))
import Data.Semigroup(Semigroup((<>)))
import Data.Set(Set)
import Data.Text(lines, Text, unlines, unwords, words)
import Data.Tuple(curry, fst, snd, swap, uncurry)

import Debug.Trace(trace, traceEvent, traceEventIO, traceId, traceIO, traceM, traceMarker, traceMarkerIO, traceShow, traceShowId, traceShowM, traceStack)

import GHC.Base(($!), seq, String)
import GHC.Err(undefined)
import GHC.Float(Double, Float)
import GHC.IO(FilePath, IO)
import GHC.Num(Integer, Num((+), (-), (*), abs, signum, fromInteger, negate), subtract)
import GHC.Real((^), (^^), Fractional((/), recip, fromRational), fromIntegral, Integral(quot, rem, div, mod, quotRem, divMod, toInteger), RealFrac(properFraction, truncate, round, ceiling, floor))
import GHC.Show(Show(show))

import Numeric(Floating, pi, exp, log, sqrt, (**), logBase, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)

import System.IO.Error(IOError, ioError, userError)

import Text.Read(read)
import Text.RegexPR(matchRegexPR)

import qualified Data.Either      as Either
import qualified Data.Foldable    as Foldable
import qualified Data.List        as List
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TIO
import qualified Data.Text.Read   as DTR
import qualified GHC.Err          as Err
import qualified System.Directory as SD
import qualified System.IO        as SIO

(|>) :: a -> (a -> b) -> b
a |> f = f a
infixr 1 |>

asString :: Text -> String
asString = Text.unpack

asPath :: Text -> FilePath
asPath = asString

asText :: String -> Text
asText = Text.pack

showText :: Show a => a -> Text
showText = show >>> asText

concat :: (Foldable t, MonadPlus m) => t (m a) -> m a
concat = Foldable.msum

error :: Text -> a
error = asString >>> Err.error

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

pam, (<&>) :: Functor f => f a -> (a -> b) -> f b
pam   = flip map
(<&>) = flip map

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct xs ys = [(x, y) | x <- xs, y <- ys]

regexMatch :: Text -> Text -> Maybe [Text]
regexMatch regex = asString >>> (matchRegexPR $ asString regex) >>> (map $ snd >>> (map $ snd >>> asText))

-- Tuple2 --

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

-- Tuple3 --

mapFst3 :: (a -> x) -> (a, b, c) -> (x, b, c)
mapFst3 f (a, b, c) = (f a, b, c)

mapSnd3 :: (b -> x) -> (a, b, c) -> (a, x, c)
mapSnd3 f (a, b, c) = (a, f b, c)

mapThd3 :: (c -> x) -> (a, b, c) -> (a, b, x)
mapThd3 f (a, b, c) = (a, b, f c)

mapAllThree :: (a -> x) -> (b -> y) -> (c -> z) -> (a, b, c) -> (x, y, z)
mapAllThree f g h (a, b, c) = (f a, g b, h c)

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

-- Tuple4 --

mapFst4 :: (a -> x) -> (a, b, c, d) -> (x, b, c, d)
mapFst4 f (a, b, c, d) = (f a, b, c, d)

mapSnd4 :: (b -> x) -> (a, b, c, d) -> (a, x, c, d)
mapSnd4 f (a, b, c, d) = (a, f b, c, d)

mapThd4 :: (c -> x) -> (a, b, c, d) -> (a, b, x, d)
mapThd4 f (a, b, c, d) = (a, b, f c, d)

mapFrt4 :: (d -> x) -> (a, b, c, d) -> (a, b, c, x)
mapFrt4 f (a, b, c, d) = (a, b, c, f d)

mapAllFour :: (a -> x) -> (b -> y) -> (c -> z) -> (d -> l) -> (a, b, c, d) -> (x, y, z, l)
mapAllFour f g h i (a, b, c, d) = (f a, g b, h c, i d)

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

-- Misc. Again --

groupOn :: Ord criterion => (item -> criterion) -> [item] -> [[item]]
groupOn f = sort >>> group
  where
    sort  = List.sortBy (compare `on` f)
    group = List.groupBy ((==) `on` f)

return' :: (Monad m) => a -> m a
return' = (return $!)

scalaGroupBy :: Ord criterion => (item -> criterion) -> [item] -> [(criterion, [item])]
scalaGroupBy f = (groupOn f) >>> pair
  where
    pair  = tee $ List.head >>> f
    tee f = map $ f &&& id

-- Hack to make GHCI print this before the prompt (JAB, 2/20/17)
putStrFlush :: Text -> IO ()
putStrFlush x = (TIO.putStr x) >>= (const $ SIO.hFlush SIO.stdout)

unsafeRead :: Integral a => Text -> a
unsafeRead = DTR.decimal >>> (Either.either (error "Well, that read *was* unsafe...") id) >>> fst

listDirsRecursively :: FilePath -> IO [FilePath]
listDirsRecursively filepath =
  do
    paths    <- SD.listDirectory filepath
    dirs     <- paths |> ((map $ \x -> filepath <> "/" <> x) >>> (filterM SD.doesDirectoryExist))
    children <- mapM listDirsRecursively dirs
    return $ dirs <> (concat children)

uncurry5 :: (a -> b -> c -> d -> e -> f) -> ((a, b, c, d, e) -> f)
uncurry5 f (a, b, c, d, e) = f a b c d e
