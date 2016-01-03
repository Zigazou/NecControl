{- |
Module      : PacketStructure
Description : Elements which structures a command packet
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Elements which structures a command packet.
-}
module Network.NecControl.PacketStructure
( PacketStructure (StartOfHeader, StartOfMessage, EndOfMessage, PacketDelimiter
                  , Reserved0
                  )
)
where

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)

{-|
The `PacketStructure` simply groups every structure code a `CommandPacket`
needs.
-}
data PacketStructure = StartOfHeader -- ^ Start of header
                     | StartOfMessage -- ^ Start of message
                     | EndOfMessage -- ^ End of message
                     | PacketDelimiter -- ^ Packet delimiter (ends a packet)
                     | Reserved0 -- ^ Reserved value of '0'
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
