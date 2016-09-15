{- |
Module      : Types
Description : Types for the commands
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Types for the commands.
-}
module Network.NecControl.Commands.Types
( ResultCode (NoError, UnsupportedOperation)
, OperationType (SetParameterType, MomentaryType)
, msgConcat
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
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

{-|
Helper function encapsulating a message between a `StartOfMessage` and
`EndOfMessage` field.
-}
msgConcat :: [[Word8]] -> [Word8]
msgConcat ws = toNec StartOfMessage ++ concat ws ++ toNec EndOfMessage
