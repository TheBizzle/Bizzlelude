module Prelude((|>), (<&>), asPath, asString, asText, concat, error, groupOn, listDirsRecursively, map, putStrFlush, return', scalaGroupBy, showText, uncurry3, uncurry4, uncurry5, unsafeRead
              , module Control.Arrow, module Control.Applicative, module Control.Monad, module Data.Bifunctor, module Data.Bool, module Data.Char, module Data.Either, module Data.Eq, module Data.Foldable, module Data.Function, module Data.Functor, module Data.Int, module Data.IntSet, module Data.Map, module Data.Maybe, module Data.Monoid, module Data.Ord, module Data.Set, module Data.Text, module Data.Tuple, module Debug.Trace, module GHC.Base, module GHC.Err, module GHC.Float, module GHC.IO, module GHC.Num, module GHC.Real, module GHC.Show, module System.IO.Error, module Text.Read) where

import Control.Arrow((&&&), (***), (>>>))
import Control.Applicative(Alternative((<|>)), Applicative((<*>), (<*), (*>), pure))
import Control.Monad((>=>), filterM, foldM, foldM_, forM, forM_, guard, join, mapM, mapM_, Monad((>>), (>>=), return), MonadPlus(), sequence, sequence_, unless, when)

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
import Data.Maybe(catMaybes, fromMaybe, isJust, isNothing, maybe, Maybe(Just, Nothing))
import Data.Monoid((<>), Monoid(mappend, mempty))
import Data.Ord(Ord((<), (<=), (>), (>=), compare, max, min), Ordering(EQ, GT, LT))
import Data.Set(Set)
import Data.Text(lines, Text, unlines, unwords, words)
import Data.Tuple(curry, fst, snd, swap, uncurry)

import Debug.Trace(trace, traceEvent, traceEventIO, traceId, traceIO, traceM, traceMarker, traceMarkerIO, traceShow, traceShowId, traceShowM, traceStack)

import GHC.Base(($!), seq, String)
import GHC.Err(undefined)
import GHC.Float(Double, Float, Floating, pi, exp, log, sqrt, (**), logBase, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)
import GHC.IO(FilePath, IO)
import GHC.Num(Integer, Num((+), (-), (*), abs, signum, fromInteger, negate), subtract)
import GHC.Real((^), Fractional((/), recip, fromRational), fromIntegral, Integral(quot, rem, div, mod, quotRem, divMod, toInteger), RealFrac(properFraction, truncate, round, ceiling, floor))
import GHC.Show(Show(show))

import System.IO.Error(IOError, ioError, userError)

import Text.Read(read)

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

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip map

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
    paths    <- SD.getDirectoryContents filepath
    dirs     <- paths |> ((map $ \x -> filepath <> "/" <> x) >>> (filterM SD.doesDirectoryExist))
    children <- mapM listDirsRecursively dirs
    return $ dirs <> (concat children)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> ((a, b, c, d, e) -> f)
uncurry5 f (a, b, c, d, e) = f a b c d e
