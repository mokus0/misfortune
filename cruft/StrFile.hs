{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
-- |Data type and serializer for fortune-mod's \"strfile\" pseudo-format.
--
-- I call it a \"pseudo-format\" because (1) it's not documented, and (2) the code that writes
-- it was not future-proof.  It looks like they are trying to make something host-independent, but
-- their attempt did not survive the advent of 64-bit systems.  The fields in the struct being
-- encoded are @unsigned long@, which in GCC on x64 is 64 bits wide, but their values are passed
-- through 'htonl' which operates on 32 bit words.
-- 
-- The result is that on 64-bit systems, the fields are 64 bits wide and contain a 32-bit 
-- big-endian word in either the lower or the upper 32 bits, depending on the endianness of
-- the host.
module Data.Fortune.StrFile (StrFileHeader(..), putStrFileHeader, getStrFileHeader) where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Serialize
import Data.Word
import Foreign.Storable (alignment)

{- 
    Notes about the format:
    
    The header is always at offset 0.  That's nice.  The structure ends with a field 
    that needs padding.  That's not nice - that means its size depends on the whims of
    the instruction set (presumably the alignment requirements for unsigned int).
    
    The offset table directly follows the header (including any padding of the last 
    field), and contains numStrings (or numStrings + 1, depending on includesPtrs)
    off_t values in the same bastardized 32-bits-in-64-bits stupid-endian format. 
 -}

-- flags:
randomFlag  = 0x1 :: Word
orderedFlag = 0x2 :: Word
rotatedFlag = 0x4 :: Word

data StrFileHeader = StrFileHeader
    { numStrings    :: !Word
    , longest       :: !Word
    , shortest      :: !Word
    , strFlags      :: !Word
    , strDelim      :: !Word8
    } deriving (Eq, Show)

includesPtrs hdr = (strFlags hdr .&. (randomFlag .|. orderedFlag) /= 0)

type StrFile = (StrFileHeader, [Int64])

-- To disentangle this mess, we apply the same operation in reverse.
-- Step 1: import some functions with deliberately wrong types.
foreign import ccall "htonl" htonl :: Word -> Word
foreign import ccall "ntohl" ntohl :: Word -> Word

putWordStupidly = putWordhost . htonl
getWordStupidly = fmap ntohl getWordhost

padding = alignment (0 :: Word) - 1
putStrFileHeader StrFileHeader{..} = do
    putWordStupidly 1 -- file format version
    putWordStupidly numStrings
    putWordStupidly longest
    putWordStupidly shortest
    putWordStupidly strFlags
    putWord8 strDelim
    replicateM_ padding (putWord8 0)
getStrFileHeader = do
    version <- getWordStupidly
    when (version /= 1) $ fail $ unwords
         [ "cannot parse strfile; version is", show version, " but we only support version 1"]
    numStrings  <- getWordStupidly
    longest     <- getWordStupidly
    shortest    <- getWordStupidly
    strFlags    <- getWordStupidly
    strDelim    <- getWord8
    replicateM_ padding getWord8
        
    return StrFileHeader{..}

putStrFile :: StrFile -> Put
putStrFile (hdr, items) = do
    putStrFileHeader hdr
    mapM_ (putWordStupidly . fromIntegral) items

getStrFile :: Get StrFile
getStrFile = do
    hdr <- getStrFileHeader
    let n = fromIntegral (numStrings hdr)
    items <- replicateM n (fmap fromIntegral getWordStupidly)
    return (hdr, items)
