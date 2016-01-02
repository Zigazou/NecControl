module Network.NecControl.NecProtocol
( NecValue (toNec, fromNec)
, necPack
, necUnpack
)
where

import qualified Data.ByteString as B
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), shiftR)

toHex :: Integral a => a -> Word8
toHex i | i < 10 = i' + 0x30
        | i < 16 = i' + 0x41 - 10
        | otherwise = error "Invalid hex digit"
        where i' = fromIntegral i

fromHex :: Integral a => Word8 -> Either String a
fromHex i | i >= 0x30 && i <= 0x39 = Right $ i' - 0x30
          | i >= 0x41 && i <= 0x46 = Right $ i' - 0x41 + 10
          | i >= 0x61 && i <= 0x66 = Right $ i' - 0x61 + 10
          | otherwise = Left $ "Invalid hex digit, ord=" ++ show i
          where i' = fromIntegral i

class NecValue a where
    toNec :: a -> [Word8]
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

necPack :: NecValue v => v -> B.ByteString
necPack = B.pack . toNec

necUnpack :: NecValue v => B.ByteString -> Either String v
necUnpack = fromNec . B.unpack
