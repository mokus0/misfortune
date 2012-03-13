{-# LANGUAGE DeriveDataTypeable #-}
module Data.Fortune.Stats where

import Data.Maybe
import Data.Monoid
import Data.Typeable

data FortuneStats = FortuneStats
    { numFortunes   :: Int
    , maxLength     :: Maybe Int
    , minLength     :: Maybe Int
    } deriving (Eq, Show)

instance Monoid FortuneStats where
    mempty = FortuneStats 0 Nothing Nothing
    mappend s1 s2 = FortuneStats
        { numFortunes = numFortunes s1 + numFortunes s2
        , maxLength   = f max (maxLength s1) (maxLength s2)
        , minLength   = f min (minLength s1) (minLength s2)
        } where
            f op Nothing y = y
            f op x Nothing = x
            f op (Just x) (Just y) = Just (op x y)

data StatsProblem
    = NegativeCount Int
    | NegativeLength Int
    | LengthsWithoutEntries
    | EntriesWithoutLengths
    | MaxLengthLessThanMinLength
    | InconsistentLengthsForOneEntry
    deriving (Eq, Ord, Read, Show, Typeable)

checkStats (FortuneStats n mx mn) = 
    case n `compare` 0 of
        LT -> Just (NegativeCount n)
        EQ -> if isNothing mx && isNothing mn 
            then Nothing
            else Just LengthsWithoutEntries
        GT -> case (mx, mn) of
            (Just mx, Just mn)
                | mx < 0    -> Just (NegativeLength mx)
                | mn < 0    -> Just (NegativeLength mx)
                | otherwise   -> case mx `compare` mn of
                LT -> Just MaxLengthLessThanMinLength
                EQ -> Nothing
                GT  | n == 1    -> Just InconsistentLengthsForOneEntry
                    | otherwise -> Nothing
            _ -> Just EntriesWithoutLengths

statsAreValid = isNothing . checkStats