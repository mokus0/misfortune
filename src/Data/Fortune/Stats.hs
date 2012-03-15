{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Fortune.Stats
    ( FortuneStats(..)
    , StatsProblem(..)
    , checkStats
    , statsAreValid
    ) where

import Data.Maybe
import Data.Semigroup
import Data.Typeable

data FortuneStats = FortuneStats
    { numFortunes   :: !(Sum Int)
    , minChars      :: !(Min Int)
    , maxChars      :: !(Max Int)
    , minLines      :: !(Min Int)
    , maxLines      :: !(Max Int)
    } deriving (Eq, Show)

wrap (a, b, c, d, e) = FortuneStats a b c d e
unwrap (FortuneStats a b c d e) = (a, b, c, d, e)

instance Semigroup FortuneStats where
    s1 <> s2 = wrap (unwrap s1 <> unwrap s2)
instance Monoid FortuneStats where
    mempty = wrap mempty; mappend = (<>)

data StatsProblem
    = NegativeCount Int
    | NegativeLength Int
    | LengthsWithoutEntries
    | EntriesWithoutLengths
    | MaxLengthLessThanMinLength
    | InconsistentLengthsForOneEntry
    deriving (Eq, Ord, Read, Show, Typeable)

checkStats FortuneStats{numFortunes = Sum n, ..} = 
    case n `compare` 0 of
        LT -> Just (NegativeCount n)
        EQ -> if all (mempty ==) [maxChars, maxLines]
              && all (mempty ==) [minChars, minLines]
            then Nothing
            else Just LengthsWithoutEntries
        GT -> getFirst 
                $  First (checkLengths minChars maxChars) 
                <> First (checkLengths minLines maxLines)
    
    where
        checkLengths (Min mn) (Max mx)
            | mx < 0    = Just (NegativeLength mx)
            | mn < 0    = Just (NegativeLength mn)
            | otherwise = case mx `compare` mn of
                LT -> Just MaxLengthLessThanMinLength
                EQ -> Nothing
                GT  | n == 1    -> Just InconsistentLengthsForOneEntry
                    | otherwise -> Nothing

statsAreValid = isNothing . checkStats