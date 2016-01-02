{-# LANGUAGE FlexibleInstances #-}
module Network.NecControl.Commands.Types
( OpCode (OpCode)
, Value (Value), valCommand, valValue, valDescription
, Constraints (ReadOnly, Values, MinMax), mimMin, mimMax, mimDescription
, Action (Action), actName, actCommand, actOpCode, actConstraints
, fromValue
, toValue
, Category (Category), catName, catActions
, Pretty, pretty
)
where

import Data.Word (Word16)
import Data.List (find)
import Control.Monad (liftM)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)

data OpCode = OpCode Word16 deriving (Eq, Show)

instance NecValue OpCode where
    toNec (OpCode o) = toNec o
    fromNec ws = liftM OpCode (fromNec ws)

data Value = Value
    { valCommand :: String
    , valValue :: Word16
    , valDescription :: String
    }

data Constraints = ReadOnly
                 | Values [Value]
                 | MinMax { mimMin :: Word16
                          , mimMax :: Word16
                          , mimDescription :: String
                          }

data Action = Action
    { actName :: String
    , actCommand :: String
    , actOpCode :: OpCode
    , actConstraints :: Constraints
    }

fromValue :: Action -> Word16 -> String
fromValue (Action _ _ _ ReadOnly) value = show value
fromValue (Action _ _ _ (Values vs)) value = human
    where human = case find (\a -> value == valValue a) vs of
                    Nothing -> ""
                    Just action -> valCommand action
fromValue (Action _ _ _ (MinMax {})) value = show value

toValue :: Action -> String -> Either String Word16
toValue (Action _ _ _ ReadOnly) str = Right $ read str
toValue (Action _ _ _ (Values vs)) str = human
    where human = case find (\a -> str == valCommand a) vs of
                    Nothing -> Left "Incorrect value"
                    Just action -> Right $ valValue action
toValue (Action _ _ _ (MinMax {})) str = Right $ read str

data Category = Category
    { catName :: String
    , catActions :: [Action]
    }

class Pretty a where
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
