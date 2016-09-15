{- |
Module      : PowerStatus
Description : Handle the powser status command and reply
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Handle the powser status command and reply.
-}
module Network.NecControl.Commands.AssetData
( 
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfMessage, EndOfMessage))
import Network.NecControl.Commands.Types (msgConcat, ResultCode)

data PowerStatusMode = PsmOn
                     | PsmStandBy
                     | PsmSuspend
                     | PsmOff
                     deriving (Eq, Show)

instance NecValue PowerStatusMode where
    toNec PsmOn = toNec (0x0001 :: Word16)
    toNec PsmStandBy = toNec (0x0002 :: Word16)
    toNec PsmSuspend = toNec (0x0003 :: Word16)
    toNec PsmOff = toNec (0x0004 :: Word16)

    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmOn
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmStandBy
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmSuspend
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmOff
    fromNec _ = Left "Invalid power status mode"

{-|
-}
data PowerStatus = PowerStatus deriving (Eq, Show)

instance NecValue PowerStatus where
    toNec PowerStatus = msgConcat [toNec (0x01d6 :: Word16)]

    fromNec _ = error "Power status from monitor is unsupported"
        
data PowerStatusReply = PowerStatusReply
    { psrPowerModeType :: Word16
    , psrCurrentPowerMode :: PowerStatusMode
    }
    deriving (Eq, Show)

instance NecValue Message where
    toNec _ = error "Power status reply from controller is unsupported"

    fromNec [ stx, 0x30, 0x32, r1, r0, 0x44, 0x36, 0x30, 0x30
            , t3, t2, t1, t0, v3, v2, v1, v0
            , etx ] = do
        som <- fromNec [stx]
        rc <- fromNec [r1, r0]
        powerModeType <- fromNec [t3, t2, t1, t0]
        powerStatusMode <- fromNec [v3, v2, v1, v0]
        eom <- fromNec [etx]

        when (som /= StartOfMessage) (Left "Invalid start of message")
        when (eom /= EndOfMessage) (Left "Invalid end of message")
        when (rc == UnsupportedOperation) (Left "Unsupported operation")
        when (rc /= NoError) (Left "Unknown error")

        return $ PowerStatus powerModeType powerStatusMode

    fromNec _ = Left "Invalid power status reply"
