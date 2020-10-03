{-# LANGUAGE NoImplicitPrelude #-}
module External(module Control.Arrow, module Control.Applicative, module Control.Monad, module Control.Monad.IO.Class, module Data.Bifunctor, module Data.Bool, module Data.Char, module Data.Either, module Data.Eq, module Data.Foldable, module Data.Function, module Data.Functor, module Data.Int, module Data.IntSet, module Data.Map, module Data.Maybe, module Data.Monoid, module Data.Ord, module Data.Semigroup, module Data.Set, module Data.Text, module Data.Traversable, module Data.Tuple, module Debug.Trace, module GHC.Base, module GHC.Enum, module GHC.Err, module GHC.Float, module GHC.IO, module GHC.Num, module GHC.Real, module GHC.Show, module Numeric, module System.IO.Error, module Text.Read) where

import Control.Arrow((&&&), (***))
import Control.Applicative(Alternative((<|>)), Applicative((<*>), (<*), (*>), pure))
import Control.Monad(filterM, foldM, foldM_, forM, forM_, guard, join, mapM, mapM_, Monad((>>), (>>=), return), MonadPlus(), sequence, sequence_, unless, when)
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
import Data.Traversable(traverse)
import Data.Tuple(curry, fst, snd, swap, uncurry)

import Debug.Trace(trace, traceEvent, traceEventIO, traceId, traceIO, traceM, traceMarker, traceMarkerIO, traceShow, traceShowId, traceShowM, traceStack)

import GHC.Base(($!), seq, String)
import GHC.Enum(Bounded, boundedEnumFrom, boundedEnumFromThen, Enum, enumFrom, enumFromThen, enumFromThenTo, enumFromTo, fromEnum, fromEnumError, maxBound, minBound, pred, predError, succ, succError, toEnum, toEnumError)

import GHC.Err(undefined)
import GHC.Float(Double, Float)
import GHC.IO(FilePath, IO)
import GHC.Num(Integer, Num((+), (-), (*), abs, signum, fromInteger, negate), subtract)
import GHC.Real((^), (^^), Fractional((/), recip, fromRational), fromIntegral, Integral(quot, rem, div, mod, quotRem, divMod, toInteger), RealFrac(properFraction, truncate, round, ceiling, floor))
import GHC.Show(Show(show))

import Numeric(Floating, pi, exp, log, sqrt, (**), logBase, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)

import System.IO.Error(IOError, ioError, userError)

import Text.Read(read)
