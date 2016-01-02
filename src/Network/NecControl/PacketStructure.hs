module Network.NecControl.PacketStructure
( PacketStructure (StartOfHeader, StartOfMessage, EndOfMessage, PacketDelimiter
                  , Reserved0
                  )
)
where

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)

data PacketStructure = StartOfHeader
                     | StartOfMessage
                     | EndOfMessage
                     | PacketDelimiter
                     | Reserved0
                     deriving (Eq, Show)

instance NecValue PacketStructure where
    toNec StartOfHeader = [ 0x01 ]
    toNec StartOfMessage = [ 0x02 ]
    toNec EndOfMessage = [ 0x03 ]
    toNec PacketDelimiter = [ 0x0d ]
    toNec Reserved0 = [ 0x30 ]

    fromNec [ 0x01 ] = Right StartOfHeader
    fromNec [ 0x02 ] = Right StartOfMessage
    fromNec [ 0x03 ] = Right EndOfMessage
    fromNec [ 0x0d ] = Right PacketDelimiter
    fromNec [ 0x30 ] = Right Reserved0
    fromNec _ = Left "Wrong packet structure"
