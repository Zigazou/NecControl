module Network.NecControl.Header
( Equipment (Monitor, Group, AllEquipment, Controller)
, MessageType ( Command, CommandReply, GetParameter, GetParameterReply
              , SetParameter, SetParameterReply
              )
, Header (Header), hdrDestination, hdrSource, hdrMsgType, hdrMsgLength
, updateLength
)
where

import Data.Char (ord, chr)
import Data.Word (Word8)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfHeader, Reserved0))

data Equipment = Monitor Int
               | Group Char
               | AllEquipment
               | Controller
               deriving (Eq, Show)

instance NecValue Equipment where
    toNec AllEquipment = [0x2a]
    toNec (Monitor i) | i < 1 || i > 100 = error "Wrong monitor ID"
                      | otherwise = [0x40 + fromIntegral i]
    toNec (Group c) | c < 'A' || c > 'J' = error "Wrong group ID"
                    | otherwise = [0x31 + fromIntegral (ord c - ord 'A')]
    toNec Controller = [0x30]

    fromNec [0x2a] = Right AllEquipment
    fromNec [0x30] = Right Controller
    fromNec [x] | x >= 0x41 && x <= 0xa4 = Right (Monitor (fromIntegral x - 0x41))
                | x >= 0x31 && x <= 0x3a = Right (Group (chr (fromIntegral x - 0x31 + ord 'A')))
                | otherwise = Left "Invalid equipment"
    fromNec _ = Left "Invalid equipment"
    
data MessageType = Command
                 | CommandReply
                 | GetParameter
                 | GetParameterReply
                 | SetParameter
                 | SetParameterReply
                 deriving (Eq, Show)

instance NecValue MessageType where
    toNec Command = [0x41]
    toNec CommandReply = [0x42]
    toNec GetParameter = [0x43]
    toNec GetParameterReply = [0x44]
    toNec SetParameter = [0x45]
    toNec SetParameterReply = [0x46]

    fromNec [0x41] = Right Command
    fromNec [0x42] = Right CommandReply
    fromNec [0x43] = Right GetParameter
    fromNec [0x44] = Right GetParameterReply
    fromNec [0x45] = Right SetParameter
    fromNec [0x46] = Right SetParameterReply
    fromNec _ = Left "Invalid message type"

data Header = Header
    { hdrDestination :: Equipment
    , hdrSource :: Equipment
    , hdrMsgType :: MessageType
    , hdrMsgLength :: Word8
    }
    deriving (Eq, Show)

updateLength :: Header -> Int -> Header
updateLength header lgth = header { hdrMsgLength = fromIntegral lgth }

instance NecValue Header where
    toNec (Header destination source msgType msgLength) = concat
        [ toNec StartOfHeader
        , toNec Reserved0
        , toNec destination
        , toNec source
        , toNec msgType
        , toNec msgLength
        ]

    fromNec [soh, resv0, destination, source, msgType, h1, h0] = do
        soh' <- fromNec [soh]
        resv0' <- fromNec [resv0]
        destination' <- fromNec [destination]
        source' <- fromNec [source]
        msgType' <- fromNec [msgType]
        msgLength <- fromNec [h1, h0]
        when (soh' /= StartOfHeader) (Left "Invalid start of header")
        when (resv0' /= Reserved0) (Left "Invalid reserved 0")
        return $ Header destination' source' msgType' msgLength

    fromNec _ = Left "Wrong header size"
