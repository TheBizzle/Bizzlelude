{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Misc((|>), (&>), (&>=), (>>>), (>=>), asString, asPath, asText, showText, concat, error, fromEither, map, pam, (<&>), cartProduct, regexMatch, groupOn, return', scalaGroupBy, putStrFlush, traceLabel, unsafeRead, uncurry5) where

import External

import Text.RegexPR(matchRegexPR)

import qualified Control.Arrow    as CArrow
import qualified Control.Monad    as CMonad
import qualified Data.Either      as Either
import qualified Data.Foldable    as Foldable
import qualified Data.List        as List
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TIO
import qualified Data.Text.Read   as DTR
import qualified GHC.Err          as Err
import qualified System.IO        as SIO

(|>) :: a -> (a -> b) -> b
a |> f = f a

(&>), (>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = (CArrow.>>>)
(&>)  = (>>>)

(&>=), (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = (CMonad.>=>)
(&>=) = (>=>)

infixl 2 >>>, >=>
infixl 1 |>

asString :: Text -> String
asString = Text.unpack

asPath :: Text -> FilePath
asPath = asString

asText :: String -> Text
asText = Text.pack

showText :: Show a => a -> Text
showText = show &> asText

concat :: (Foldable t, MonadPlus m) => t (m a) -> m a
concat = Foldable.msum

error :: Text -> a
error = asString &> Err.error

map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

pam, (<&>) :: Functor f => f a -> (a -> b) -> f b
pam   = flip map
(<&>) = flip map

cartProduct :: [a] -> [b] -> [(a, b)]
cartProduct xs ys = [(x, y) | x <- xs, y <- ys]

regexMatch :: Text -> Text -> Maybe [Text]
regexMatch regex = asString &> (matchRegexPR $ asString regex) &> (map $ snd &> (map $ snd &> asText))

fromEither :: Either a a -> a
fromEither = either id id

groupOn :: Ord criterion => (item -> criterion) -> [item] -> [[item]]
groupOn f = sort &> group
  where
    sort  = List.sortBy (compare `on` f)
    group = List.groupBy ((==) `on` f)

return' :: (Monad m) => a -> m a
return' = (return $!)

scalaGroupBy :: Ord criterion => (item -> criterion) -> [item] -> [(criterion, [item])]
scalaGroupBy f = (groupOn f) &> pair
  where
    pair  = tee $ List.head &> f
    tee f = map $ f &&& id

-- Hack to make GHCI print this before the prompt (JAB, 2/20/17)
putStrFlush :: Text -> IO ()
putStrFlush x = (TIO.putStr x) >>= (const $ SIO.hFlush SIO.stdout)

traceLabel :: (Show a) => Text -> a -> a
traceLabel label a = traceShow (label <> ": " <> (showText a)) a

unsafeRead :: Integral a => Text -> a
unsafeRead = DTR.decimal &> (Either.either (error "Well, that read *was* unsafe...") id) &> fst

uncurry5 :: (a -> b -> c -> d -> e -> f) -> ((a, b, c, d, e) -> f)
uncurry5 f (a, b, c, d, e) = f a b c d e
