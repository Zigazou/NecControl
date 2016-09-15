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
          , repOpCode, repOpType, repMaxValue, repValue, MsgPowerStatus
          , MsgPowerStatusReply, MsgPowerControl, mpcPowerMode
          , MsgPowerControlReply, MsgAssetDataRead, marStart, marLength
          , MsgAssetDataReadReply, MsgAssetDataWrite, mawStart, mawData
          , MsgAssetDataWriteReply, MsgDateTimeRead, MsgDateTimeReadReply
          , MsgDateTimeWrite, mdwYear, mdwMonth, mdwDay, mdwWeekDay, mdwHour
          , mdwMinute, mdwDaylightSaving, MsgDateTimeWriteReply
          , MsgScheduleRead, msrProgram, MsgScheduleReadReply, MsgScheduleWrite
          , mswProgram, mswOnHour, mswOnMinute, mswOffHour, mswOffMinute
          , mswInput, mswWeekDay, mswFrequency, mswPictureMode, mswExt1
          , mswExt2, mswExt3, mswExt4, mswExt5, mswExt6, mswExt7
          , MsgScheduleWriteReply, MsgSelfDiagnosis, MsgSelfDiagnosisReply
          , MsgSerialNo, MsgSerialNoReply, MsgModelName, MsgModelNameReply
          )
, msgConcat
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfMessage, EndOfMessage))
import Network.NecControl.Parameters.Types (OpCode)

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

data PowerMode = PwcOn
               | PwcOff
               deriving (Eq, Show)

instance NecValue PowerMode where
    toNec PwcOn = toNec (0x0001 :: Word16)
    toNec PwcOff = toNec (0x0004 :: Word16)

    fromNec [0x30, 0x30, 0x30, 0x31] = Right PwcOn
    fromNec [0x30, 0x30, 0x30, 0x34] = Right PwcOff
    fromNec _ = Left "Invalid power mode value"
    
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
data Message = MsgGetParameter
                { mgpOpCode :: OpCode }
             | MsgSetParameter
                { mspOpCode :: OpCode
                , mspValue :: Word16
                }
             | MsgParameterReply
                { repResultCode :: ResultCode -- ^ the result code
                , repOpCode :: OpCode -- ^ an operation code
                , repOpType :: OperationType -- ^ an operation type
                , repMaxValue :: Word16 -- ^ max supported value
                , repValue :: Word16 -- ^ current or set value
                }
             | MsgPowerStatus
             | MsgPowerStatusReply
             | MsgPowerControl
                { mpcPowerMode :: PowerMode }
             | MsgPowerControlReply
             | MsgAssetDataRead
                { marStart :: Word8
                , marLength :: Word8
                }
             | MsgAssetDataReadReply
             | MsgAssetDataWrite
                { mawStart :: Word8
                , mawData :: String
                }
             | MsgAssetDataWriteReply
             | MsgDateTimeRead
             | MsgDateTimeReadReply
             | MsgDateTimeWrite
                { mdwYear :: Word8
                , mdwMonth :: Word8
                , mdwDay :: Word8
                , mdwWeekDay :: Word8
                , mdwHour :: Word8
                , mdwMinute :: Word8
                , mdwDaylightSaving :: Word8
                }
             | MsgDateTimeWriteReply
             | MsgScheduleRead
                { msrProgram :: Word8 }
             | MsgScheduleReadReply
             | MsgScheduleWrite
                { mswProgram :: Word8
                , mswOnHour :: Word8
                , mswOnMinute :: Word8
                , mswOffHour :: Word8
                , mswOffMinute :: Word8
                , mswInput :: Word8
                , mswWeekDay :: Word8
                , mswFrequency :: Word8
                , mswPictureMode :: Word8
                , mswExt1 :: Word8
                , mswExt2 :: Word8
                , mswExt3 :: Word8
                , mswExt4 :: Word8
                , mswExt5 :: Word8
                , mswExt6 :: Word8
                , mswExt7 :: Word8
                }
             | MsgScheduleWriteReply
             | MsgSelfDiagnosis
             | MsgSelfDiagnosisReply
             | MsgSerialNo
             | MsgSerialNoReply
             | MsgModelName
             | MsgModelNameReply
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

    toNec MsgPowerStatus = msgConcat [ toNec (0x1d6 :: Word16) ]
    toNec (MsgPowerControl powerMode) = msgConcat
        [ toNec (0xc203 :: Word16)
        , toNec (0xd6 :: Word8)
        , toNec powerMode
        ]
    toNec (MsgAssetDataRead start lgth) = msgConcat
        [ toNec (0xc00b :: Word16)
        , toNec start
        , toNec lgth
        ]
    toNec (MsgAssetDataWrite start asset) = msgConcat
        [ toNec (0xc00e :: Word16)
        , toNec start
        , toNec asset
        ]

    toNec MsgDateTimeRead = msgConcat [ toNec (0xc211 :: Word16) ]
    
    toNec msg@(MsgDateTimeWrite {}) = msgConcat
        [ toNec (0xc212 :: Word16)
        , toNec $ mdwYear msg
        , toNec $ mdwMonth msg
        , toNec $ mdwDay msg
        , toNec $ mdwWeekDay msg
        , toNec $ mdwHour msg
        , toNec $ mdwMinute msg
        , toNec $ mdwDaylightSaving msg
        ]

    toNec (MsgScheduleRead program) = msgConcat
        [ toNec (0xc221 :: Word16)
        , toNec program
        ]

    toNec msg@(MsgScheduleWrite {}) = msgConcat
        [ toNec (0xc222 :: Word16)
        , toNec $ mswProgram msg
        , toNec $ mswOnHour msg
        , toNec $ mswOnMinute msg
        , toNec $ mswOffHour msg
        , toNec $ mswOffMinute msg
        , toNec $ mswInput msg
        , toNec $ mswWeekDay msg
        , toNec $ mswFrequency msg
        , toNec $ mswPictureMode msg
        , toNec $ mswExt1 msg
        , toNec $ mswExt2 msg
        , toNec $ mswExt3 msg
        , toNec $ mswExt4 msg
        , toNec $ mswExt5 msg
        , toNec $ mswExt6 msg
        , toNec $ mswExt7 msg
        ]

    toNec MsgSelfDiagnosis = msgConcat [ toNec (0xb1 :: Word8) ]
    toNec MsgSerialNo = msgConcat [ toNec (0xc216 :: Word16) ]
    toNec MsgModelName = msgConcat [ toNec (0xc217 :: Word16) ]

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
