{- |
Module      : CommandPacket
Description : Generate or read a complete command packet from Nec monitor
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Generate or read a complete command packet from Nec monitor.
-}
module Network.NecControl.CommandPacket
( CheckCode (CheckCode)
, CommandPacket, cmpHeader, cmpMessage, cmpCheckCode
, mkPacket
)
where

import Data.Word (Word8)
import Data.Bits (xor)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.Header
    ( Header (Header), hdrMsgLength, hdrMsgType, hdrDestination, hdrSource
    , MessageType (Command, CommandReply, GetParameter, GetParameterReply
                  , SetParameter, SetParameterReply
                  )
    , updateLength, Equipment
    )
import Network.NecControl.Message (Message (MsgGetParameter, MsgSetParameter))
import Network.NecControl.PacketStructure (PacketStructure(PacketDelimiter))

{-|
A `CheckCode` is a control code inserted in a `CommandPacket` to ensure
everything is ok. It is simply a `xor` of all values of the `Header` and the
`Message`, except for the `StartOfHeader`.
-}
data CheckCode = CheckCode Word8 deriving (Eq, Show)

instance NecValue CheckCode where
    toNec (CheckCode v) = [v]
    fromNec [v] = Right (CheckCode v)
    fromNec _ = Left "Invalid check code"

{-|
A `CommandPacket` is a complete packet sent from the controller to the monitor
or from the monitor to the controller. It contains an `Header`, a `Message`
and additional data such as a `CheckCode` and a `PacketDelimiter` (this one
being automatically generated).
-}
data CommandPacket = CommandPacket
    { cmpHeader :: Header -- ^ the header of the packet
    , cmpMessage :: Message -- ^ the message of the packet
    , cmpCheckCode :: CheckCode -- ^ the checkcode (calculated by preparePacket)
    } deriving (Eq, Show)

instance NecValue CommandPacket where
    toNec (CommandPacket header message checkCode) = concat
        [ toNec header
        , toNec message
        , toNec checkCode
        , toNec PacketDelimiter
        ]

    fromNec ws@(_:_:_:_:_:_:_:_:_:_:_:_:_:_) = do
        header <- fromNec (take 7 ws)

        let msgLength = fromIntegral (hdrMsgLength header)
            waitedLength = 7 -- header
                         + msgLength
                         + 2 -- check code and packet delimiter

        when (length ws /= waitedLength) (Left "Incomplete command packet")

        pckDlm <- fromNec [last ws]
        when (pckDlm /= PacketDelimiter) (Left "Wrong packet delimiter")

        let msgNec = take msgLength (drop 7 ws)
        message <- case hdrMsgType header of
            Command -> Left "TODO: support command message type"
            CommandReply -> Left "TODO: support command reply message type"
            GetParameter -> fromNec msgNec
            GetParameterReply -> fromNec msgNec
            SetParameter -> fromNec msgNec
            SetParameterReply -> fromNec msgNec

        let checkCode = CheckCode $ last (init ws)

        return $ CommandPacket header message checkCode

    fromNec _ = Left "Incomplete command packet"

{-|
Updates the length field and the check code of a `CommandPacket` in order to
get a completely valid `CommandPacket`.
-}
preparePacket :: CommandPacket -> CommandPacket
preparePacket (CommandPacket header message _) =
    CommandPacket header' message (CheckCode checkCode)
    where header' = updateLength header (length $ toNec message)
          checkCode = foldl1 xor $ tail (toNec header') ++ toNec message

{-|
Generate a completely valid `CommandPacket`. It is primarily meant to send
from the controller to a monitor.
-}
mkPacket :: Equipment -- ^ Receiver (ie. a monitor)
         -> Equipment -- ^ Sender (ie. the controller)
         -> Message -- ^ Message
         -> CommandPacket -- ^ A valid command packet
mkPacket receiver sender message =
    preparePacket $ CommandPacket header message (CheckCode 0x00)
    where msgType = case message of
                        (MsgGetParameter _) -> GetParameter
                        (MsgSetParameter _ _) -> SetParameter
                        _ -> error "Only Get and Set message type are allowed"

          header = Header { hdrDestination = receiver
                          , hdrSource = sender
                          , hdrMsgType = msgType
                          , hdrMsgLength = 0x00
                          }
