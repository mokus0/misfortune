{-# LANGUAGE DeriveDataTypeable #-}
module Data.Fortune.Stats where

import Data.Maybe
import Data.Monoid
import Data.Typeable

data FortuneStats = FortuneStats
    { numFortunes   :: Int
    , maxChars     :: Maybe Int
    , minChars     :: Maybe Int
    , maxLines     :: Maybe Int
    , minLines     :: Maybe Int
    } deriving (Eq, Show)

instance Monoid FortuneStats where
    mempty = FortuneStats 0 Nothing Nothing Nothing Nothing
    mappend s1 s2 = FortuneStats
        { numFortunes = numFortunes s1 + numFortunes s2
        , maxChars   = f max (maxChars s1) (maxChars s2)
        , minChars   = f min (minChars s1) (minChars s2)
        , maxLines   = f max (maxLines s1) (maxLines s2)
        , minLines   = f min (minLines s1) (minLines s2)
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

checkStats (FortuneStats n mxc mnc mxl mnl) = 
    case n `compare` 0 of
        LT -> Just (NegativeCount n)
        EQ -> if all isNothing [mxc, mnc, mxl, mnl]
            then Nothing
            else Just LengthsWithoutEntries
        GT -> getFirst $ mappend
            (First $ checkLengths (mxc, mnc))
            (First $ checkLengths (mxl, mnl))
    
    where
        checkLengths (Just mx, Just mn)
            | mx < 0    = Just (NegativeLength mx)
            | mn < 0    = Just (NegativeLength mx)
            | otherwise = case mx `compare` mn of
                LT -> Just MaxLengthLessThanMinLength
                EQ -> Nothing
                GT  | n == 1    -> Just InconsistentLengthsForOneEntry
                    | otherwise -> Nothing
        checkLengths _ = Just EntriesWithoutLengths

statsAreValid = isNothing . checkStats