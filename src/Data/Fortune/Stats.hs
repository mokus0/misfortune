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

-- |Some statistics about the fortunes in a database.  These are stored in 
-- the index file and used to speed up various calculations that would otherwise
-- require re-reading lots of files.
data FortuneStats = FortuneStats
    { numFortunes   :: !(Sum Int)
    , offsetAfter   :: !(Max Int)
    , minChars      :: !(Min Int)
    , maxChars      :: !(Max Int)
    , minLines      :: !(Min Int)
    , maxLines      :: !(Max Int)
    } deriving (Eq, Show)

wrap (a, b, c, (d, e, f)) = FortuneStats a b c d e f
unwrap (FortuneStats a b c d e f) = (a, b, c, (d, e, f))

instance Semigroup FortuneStats where
    s1 <> s2 = wrap (unwrap s1 <> unwrap s2)
instance Monoid FortuneStats where
    mempty = wrap mempty; mappend = (<>)

-- |Errors that can be thrown when stats are read from an index file.
-- These errors describe various logical inconsistencies that generally
-- indicate that the index file is corrupted somehow.
data StatsProblem
    = NegativeCount !Int
    | NegativeLength !Int
    | NegativeOffset !Int
    | LengthsWithoutEntries
    | EntriesWithoutLengths
    | MaxLengthLessThanMinLength
    | InconsistentLengthsForOneEntry
    deriving (Eq, Ord, Read, Show, Typeable)

checkStats FortuneStats{numFortunes = Sum n, offsetAfter = Max o, ..}
    | n > 0 && o < 0    = Just (NegativeOffset o)
    | otherwise         = case n `compare` 0 of
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