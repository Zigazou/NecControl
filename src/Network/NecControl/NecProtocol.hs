{- |
Module      : NecProtocol
Description : Base module for handling the Nec protocol
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Base module for handling the Nec protocol to send/packets to/from a Nec LCD
monitor.
-}
module Network.NecControl.NecProtocol
( NecValue (toNec, fromNec)
, necPack
, necUnpack
)
where

import qualified Data.ByteString as B
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftR)

{-|
Converts a number between 0 and 15 to an hexadecimal digit (ASCII).
-}
toHex :: Integral a => a -> Word8
toHex i | i < 10 = i' + 0x30
        | i < 16 = i' + 0x41 - 10
        | otherwise = error "Invalid hex digit"
        where i' = fromIntegral i

{-|
Converts an hexadecimal digit (ASCII) to a number between 0 and 15.
-}
fromHex :: Integral a => Word8 -> Either String a
fromHex i | i >= 0x30 && i <= 0x39 = Right $ i' - 0x30
          | i >= 0x41 && i <= 0x46 = Right $ i' - 0x41 + 10
          | i >= 0x61 && i <= 0x66 = Right $ i' - 0x61 + 10
          | otherwise = Left $ "Invalid hex digit, ord=" ++ show i
          where i' = fromIntegral i

{-|
The `NecValue` class converts any value to/from a valid sequence of bytes
according to the Nec protocol.
-}
class NecValue a where
    {-|
    Converts any value to a sequence of bytes ready for the Nec protocol.
    -}
    toNec :: a -> [Word8]

    {-|
    Converts a sequence of bytes from the Nec protocol to any value.
    -}
    fromNec :: [Word8] -> Either String a

instance NecValue Word8 where
    toNec i = toHex <$> [ h1, h0 ]
        where h1 = i `shiftR` 4
              h0 = i .&. 0x0f

    fromNec [h1, h0] = do
        eh1 <- fromHex h1
        eh0 <- fromHex h0
        return $ eh1 * 16 + eh0

    fromNec _ = Left "Wrong number of hex digits, was waiting for 2"

instance NecValue Word16 where
    toNec i = toHex <$> [ h3, h2, h1, h0 ]
        where h3 = (i .&. 0xf000) `shiftR` 12
              h2 = (i .&. 0x0f00) `shiftR` 8
              h1 = (i .&. 0x00f0) `shiftR` 4
              h0 = i .&. 0x000f

    fromNec [h3, h2, h1, h0] = do
        eh3 <- fromHex h3
        eh2 <- fromHex h2
        eh1 <- fromHex h1
        eh0 <- fromHex h0
        return $ eh3 * 4096 + eh2 * 256 + eh1 * 16 + eh0

    fromNec _ = Left "Wrong number of hex digits, was waiting for 4"

{-|
Helper function: converts a value to a `ByteString` ready to be sent to a Nec
LCD monitor.
-}
necPack :: NecValue v => v -> B.ByteString
necPack = B.pack . toNec

{-|
Helper function: converts a `ByteString` received from a Nec LCD monitor to any
value.
-}
necUnpack :: NecValue v => B.ByteString -> Either String v
necUnpack = fromNec . B.unpack
