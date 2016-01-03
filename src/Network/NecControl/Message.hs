{- |
Module      : Message
Description : Generate or read a message of a command packet
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Generate or read a message of a command packet.
-}
module Network.NecControl.Message
( ResultCode (NoError, UnsupportedOperation)
, OperationType (SetParameterType, MomentaryType)
, Message ( MsgGetParameter, MsgSetParameter, MsgParameterReply, repResultCode
          , repOpCode, repOpType, repMaxValue, repValue
          )
, msgConcat
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfMessage, EndOfMessage))
import Network.NecControl.Commands.Types (OpCode)

{-|
A `ResultCode` is used when a `Message` is a reply. It indicates whether an
request generated an error or not.
-}
data ResultCode = NoError -- ^ Everything went fine
                | UnsupportedOperation -- ^ Unsupported operation
                deriving (Eq, Show)

instance NecValue ResultCode where
    toNec NoError = [0x30, 0x30]
    toNec UnsupportedOperation = [0x30, 0x31]

    fromNec [0x30, 0x30] = Right NoError
    fromNec [0x30, 0x31] = Right UnsupportedOperation
    fromNec _ = Left "Invalid result code"

{-|
An `OperationType` indicates if a reply if a set parameter type or a momentary
type.
-}
data OperationType = SetParameterType
                   | MomentaryType
                   deriving (Eq, Show)

instance NecValue OperationType where
    toNec SetParameterType = [0x30, 0x30]
    toNec MomentaryType = [0x30, 0x31]

    fromNec [0x30, 0x30] = Right SetParameterType
    fromNec [0x30, 0x31] = Right MomentaryType
    fromNec _ = Left "Invalid operation type"

{-|
A `Message` has different forms. It can be:

- a Get Parameter message
- a Set Parameter message
- a reply to either a Get or Set Parameter message

-}
data Message = MsgGetParameter OpCode
             | MsgSetParameter OpCode Word16
             | MsgParameterReply
                { repResultCode :: ResultCode -- ^ the result code
                , repOpCode :: OpCode -- ^ an operation code
                , repOpType :: OperationType -- ^ an operation type
                , repMaxValue :: Word16 -- ^ max supported value
                , repValue :: Word16 -- ^ current or set value
                }
             deriving (Eq, Show)

{-|
Helper function encapsulating a message between a `StartOfMessage` and
`EndOfMessage` field.
-}
msgConcat :: [[Word8]] -> [Word8]
msgConcat ws = toNec StartOfMessage ++ concat ws ++ toNec EndOfMessage

instance NecValue Message where
    toNec (MsgGetParameter opCode) = msgConcat [toNec opCode]

    toNec (MsgSetParameter opCode value) = msgConcat
        [ toNec opCode
        , toNec value
        ]

    toNec (MsgParameterReply rc oc mt maxValue currentValue) = msgConcat
        [ toNec rc
        , toNec oc
        , toNec mt
        , toNec maxValue
        , toNec currentValue
        ]

    fromNec [ stx, oc3, oc2, oc1, oc0, etx ] = do
        som <- fromNec [stx]
        opCode <- fromNec [oc3, oc2, oc1, oc0]
        eom <- fromNec [etx]

        when (som /= StartOfMessage) (Left "Invalid start of message")
        when (eom /= EndOfMessage) (Left "Invalid end of message")

        return $ MsgGetParameter opCode

    fromNec [ stx, oc3, oc2, oc1, oc0, v3, v2, v1, v0, etx ] = do
        som <- fromNec [stx]
        opCode <- fromNec [oc3, oc2, oc1, oc0]
        value <- fromNec [v3, v2, v1, v0]
        eom <- fromNec [etx]

        when (som /= StartOfMessage) (Left "Invalid start of message")
        when (eom /= EndOfMessage) (Left "Invalid end of message")

        return $ MsgSetParameter opCode value

    fromNec [ stx
            , r1, r0
            , oc3, oc2, oc1, oc0
            , t1, t0
            , mx3, mx2, mx1, mx0
            , v3, v2, v1, v0
            , etx
            ] = do
                    som <- fromNec [stx]
                    resultCode <- fromNec [r1, r0]
                    opCode <- fromNec [oc3, oc2, oc1, oc0]
                    operationType <- fromNec [t1, t0]
                    maxValue <- fromNec [mx3, mx2, mx1, mx0]
                    currentValue <- fromNec [v3, v2, v1, v0]
                    eom <- fromNec [etx]

                    when (som /= StartOfMessage)
                         (Left "Invalid start of message")

                    when (eom /= EndOfMessage)
                         (Left "Invalid end of message")
  
                    return $ MsgParameterReply resultCode
                                               opCode
                                               operationType
                                               maxValue
                                               currentValue

    fromNec _ = Left "Invalid message"
