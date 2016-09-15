{- |
Module      : ModelName
Description : Handle the model name command and reply
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Handle the model name command and reply.
-}
module Network.NecControl.Commands.ModelName
( 
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfMessage, EndOfMessage))
import Network.NecControl.Commands.Types (msgConcat, ResultCode)

data ModelNameMode = PsmOn
                     | PsmStandBy
                     | PsmSuspend
                     | PsmOff
                     deriving (Eq, Show)

instance NecValue ModelNameMode where
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
data ModelName = ModelName deriving (Eq, Show)

instance NecValue ModelName where
    toNec ModelName = msgConcat [toNec (0xc217 :: Word16)]

    fromNec _ = error "Model name from monitor is unsupported"
        
data ModelNameReply = ModelNameReply
    { mnrModelName :: [Word8] }
    deriving (Eq, Show)

instance NecValue ModelNameReply where
    toNec _ = error "Model name reply from controller is unsupported"

    fromNec (stx:0x43:0x33:0x31:0x37:mns) = do
        som <- fromNec [stx]
        modelName <- fromNec $ init mns
        eom <- fromNec $ last mns

        when (som /= StartOfMessage) (Left "Invalid start of message")
        when (eom /= EndOfMessage) (Left "Invalid end of message")

        return $ ModelName powerModeType powerStatusMode

    fromNec _ = Left "Invalid power status reply"
