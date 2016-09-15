{- |
Module      : PowerControl
Description : Handle the powser control command and reply
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Handle the powser control command and reply.
-}
module Network.NecControl.Commands.PowerControl
( PowerStatus ( PowerStatus )
, PowerStatusReply (PowerStatusReply
, msgConcat
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfMessage, EndOfMessage))
import Network.NecControl.Commands.Types (msgConcat, ResultCode)

data PowerMode = PmOn
               | PmOff
               deriving (Eq, Show)

instance NecValue PowerMode where
    toNec PsmOn = toNec (0x0001 :: Word16)
    toNec PsmOff = toNec (0x0004 :: Word16)

    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmOn
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmOff
    fromNec _ = Left "Invalid power mode"

{-|
-}
data PowerControl = PowerControl
    { pocPowerMode :: PowerMode }
    deriving (Eq, Show)

instance NecValue PowerControl where
    toNec (PowerControl powerMode) = msgConcat
        [ toNec StartOfMessage
        , toNec (0xc203 :: Word16)
        , toNec (0xd6 :: Word8)
        , toNec powerMode
        , toNec EndOfMessage
        ]

    fromNec _ = error "Power control from monitor is unsupported"
        
data PowerStatusReply = PowerStatusReply
    { psrResultCode :: ResultCode
    , psrCurrentPowerMode :: PowerMode
    }
    deriving (Eq, Show)

instance NecValue PowerControlReply where
    toNec _ = error "Power control reply from controller is unsupported"

    fromNec [ stx, r1, r0, 0x43, 0x32, 0x30, 0x33, 0x44, 0x36
            , v3, v2, v1, v0
            , etx ] = do
        som <- fromNec [stx]
        rc <- fromNec [r1, r0]
        powerMode <- fromNec [v3, v2, v1, v0]
        eom <- fromNec [etx]

        when (som /= StartOfMessage) (Left "Invalid start of message")
        when (eom /= EndOfMessage) (Left "Invalid end of message")
        when (rc == UnsupportedOperation) (Left "Unsupported operation")
        when (rc /= NoError) (Left "Unknown error")

        return $ PowerControlReply rc powerMode

    fromNec _ = Left "Invalid power control reply"
