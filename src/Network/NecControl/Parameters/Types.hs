{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Types
Description : Types needed to handle Get/Set Parameter message
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Generate or read a complete command packet from Nec monitor.
-}
module Network.NecControl.Parameters.Types
( OpCode (OpCode)
, Value (Value, valCommand, valValue, valDescription)
, Constraints (ReadOnly, Values, MinMax, mimMin, mimMax, mimDescription)
, Action (Action, actName, actCommand, actOpCode, actConstraints)
, fromValue
, toValue
, Category (Category, catName, catActions)
, Pretty (pretty)
)
where

import Data.Word (Word16)
import Data.List (find)
import Control.Monad (liftM)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)

{-|
An `OpCode` is a 16 bits value defined by the Nec protocol to determine an
operation.
-}
data OpCode = OpCode Word16 deriving (Eq, Show)

instance NecValue OpCode where
    toNec (OpCode o) = toNec o
    fromNec ws = liftM OpCode (fromNec ws)

{-|
A `Value` is a structure which allows to humanize a raw value the Nec protocol
defines. For example, 'dynamic' is the humanized value of 0x06 for the Aspect
opcode.
-}
data Value = Value
    { valCommand :: String -- ^ Humanized value
    , valValue :: Word16 -- ^ Raw value
    , valDescription :: String -- ^ Simple help or note for the end user
    }

{-|
A `Constraints` sets the rules along which a `Value` must be handled.
-}
data Constraints = ReadOnly -- ^ The value cannot be set, only read
                 | Values [Value] -- ^ List of specific values
                 | MinMax
                    { mimMin :: Word16 -- ^ minimum value (included)
                    , mimMax :: Word16 -- ^ maximum value (included)
                    , mimDescription :: String -- ^ Simple help for the user
                    }

{-|
An `Action` designates a command a Nec monitor can handle.
-}
data Action = Action
    { actName :: String -- ^ Description of the action
    , actCommand :: String -- ^ Humanized name of the action
    , actOpCode :: OpCode -- ^ Opcode of the action
    , actConstraints :: Constraints -- ^ Value constraints
    }

{-|
Given an `Action` and a `Value`, returns the humanized value.
-}
fromValue :: Action -> Word16 -> String
fromValue (Action _ _ _ ReadOnly) value = show value
fromValue (Action _ _ _ (Values vs)) value = human
    where human = case find (\a -> value == valValue a) vs of
                    Nothing -> ""
                    Just action -> valCommand action
fromValue (Action _ _ _ (MinMax {})) value = show value

{-|
Given an `Action` and a humanized value, returns its corresponding value.
-}
toValue :: Action -> String -> Either String Word16
toValue (Action _ _ _ ReadOnly) str = Right $ read str
toValue (Action _ _ _ (Values vs)) str = human
    where human = case find (\a -> str == valCommand a) vs of
                    Nothing -> Left "Incorrect value"
                    Just action -> Right $ valValue action
toValue (Action _ _ _ (MinMax {})) str = Right $ read str

{-|
A `Category` only helps organizing a very long list of actions.
-}
data Category = Category
    { catName :: String -- ^ Category name
    , catActions :: [Action] -- ^ List of `Action` grouped by the category
    }

{-|
The `Pretty` class helps generating an end user help to be displayed on a
command line.
-}
class Pretty a where
    {-|
    Returns a list of strings which can then be unlines later.
    -}
    pretty :: a -> [String]

instance Pretty Value where
    pretty (Value c _ "") = [ c ]
    pretty (Value c _ d) = [ c ++ " (" ++ d ++ ")" ]

instance Pretty Constraints where
    pretty ReadOnly = [ "read-only" ]
    pretty (MinMax mi ma "") =
        [ "Range: [" ++ show mi ++ ".." ++ show ma ++ "]" ]
    pretty (MinMax mi ma d) =
        [ "Range: [" ++ show mi ++ ".." ++ show ma ++ "] (" ++ d ++ ")" ]
    pretty (Values vs) = concat (pretty <$> vs)

instance Pretty Action where
    pretty a = (actCommand a ++ " - " ++ actName a)
             : (("    " ++) <$> pretty (actConstraints a))

instance Pretty Category where
    pretty (Category name actions) =
        name : (("    " ++) <$> pretty actions)

instance Pretty p => Pretty [p] where
    pretty = concatMap pretty
