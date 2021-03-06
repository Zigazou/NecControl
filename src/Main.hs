{- |
Module      : Main
Description : Command line utility neccontrol
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This program allows you to control your Nec LCD monitor from a Unix command
line.
-}
module Main where

import Data.List (find)
import System.Environment (getArgs)
import Network.Simple.TCP
import Data.Word (Word16)
import Data.Char (ord)
import Data.ByteString (ByteString, append)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))

import Hexdump (prettyHex)

import Network.NecControl.Parameters.Types
import Network.NecControl.Parameters.All (allParameters)
import Network.NecControl.Header
import Network.NecControl.NecProtocol
import Network.NecControl.CommandPacket
import Network.NecControl.Message

import Control.Monad.Except

{-|
Handles exceptions in the IO monad.
-}
type NecControlIO = ExceptT String IO

{-|
PutStrLn version for using in the `NecControlIO` monad.
-}
ncPutStrLn :: String -> NecControlIO ()
ncPutStrLn = liftIO . putStrLn

{-|
Lift the `Either` monad to the `NecControlIO` monad, making it easier to use
the `Either` type without resorting to multiple case statement.
-}
liftE :: Either String a -> NecControlIO a
liftE e = case e of
               Left msg -> throwError msg
               Right v -> return v

{-|
Lift the `Maybe` monad to the `NecControlIO` monad, making it easier to use
the `Maybe` type without resorting to multiple case statement.
-}
liftMaybe :: String -> Maybe a -> NecControlIO a
liftMaybe msg mba = case mba of
                         Nothing -> throwError msg
                         Just v -> return v

{-|
Get all available actions in one list.
-}
getActions :: [Category] -> [Action]
getActions = concatMap catActions

{-|
Find an `Action` in a list of Action given its humanized name.
-}
findActionByCmd :: String -> [Action] -> Either String Action
findActionByCmd str actions =
    case find (\a -> str == actCommand a) actions of
         Nothing -> Left "Unknown command"
         Just action -> Right action

{-|
Basic help for the end user.
-}
simpleHelp :: String
simpleHelp = unlines
    [ "Usage:"
    , "  neccontrol"
    , "      this simple help"
    , ""
    , "  neccontrol help [command]"
    , "      ask complete help (quite long) or help about a command"
    , ""
    , "  neccontrol <ipAddress> <monitorId> get <command>"
    , "      get current parameter value from a specific monitor"
    , ""
    , "  neccontrol <ipAddress> <monitorId> set <command> <value>"
    , "      set parameter of a specific monitor"
    , ""
    , "The neccontrol utility allows to send commands to a Nec Monitor."
    , "It currently supports only TCP/IP connection, not RS-232C."
    , "Note: Every name is case sensitive."
    ]

{-|
Complete help for the end user (includes basic help).
-}
allCommandHelp :: String
allCommandHelp = unlines $ "List of available commands:" : pretty allParameters

{-|
Parse arguments from the command line and run commands.
-}
parseArgs :: [String] -> NecControlIO ()
parseArgs [ "help" ] = ncPutStrLn $ unlines [simpleHelp, "", allCommandHelp]

parseArgs [ "help", command ] = do
    action <- liftE $ findActionByCmd command (getActions allParameters)
    ncPutStrLn $ unlines $ pretty action

parseArgs (ipAddress:[monitorId]:todo) = do
    equipment <- liftE $ fromNec [(fromIntegral . ord) monitorId]
    execute ipAddress equipment todo

parseArgs _ = ncPutStrLn simpleHelp

{-|
Execute a command.
-}
execute :: String -> Equipment -> [String] -> NecControlIO ()
execute _ _ [] = return ()

execute host target ("get":command:_) = do
    action <- liftE $ findActionByCmd command (getActions allParameters)
    (monitor, _) <- liftIO $ connectSock host "7142"
    value <- doGetParameter monitor target action
    ncPutStrLn $ fromValue action value
    liftIO $ closeSock monitor

execute host target ("set":command:value:_) = do
    action <- liftE $ findActionByCmd command (getActions allParameters)
    value' <- liftE $ toValue action value
    (monitor, _) <- liftIO $ connectSock host "7142"
    newValue <- doSetParameter monitor target action value'
    ncPutStrLn $ fromValue action newValue
    liftIO $ closeSock monitor

execute _ _ _ = ncPutStrLn simpleHelp >> throwError "Invalid command"

{-|
Nec monitor seems to send packet in two steps.
-}
necRecv :: Socket -> ExceptT String IO ByteString
necRecv monitor = do
    reply1 <- recv monitor 64
    reply2 <- recv monitor 64
    bin1 <- liftMaybe "Monitor did not reply" reply1
    bin2 <- liftMaybe "Monitor did not reply" reply2
    return $ append bin1 bin2

{-|
Run a Get Parameter command
-}
doGetParameter :: Socket -> Equipment -> Action -> NecControlIO Word16
doGetParameter monitor target action = do
    let command = mkPacket target
                           Controller
                           (MsgGetParameter $ actOpCode action)

    send monitor (necPack command)
    bin <- necRecv monitor
    case necUnpack bin of
         Left msg -> throwError $ msg ++ "\n" ++ prettyHex bin
         Right packet -> return $ repValue (cmpMessage packet)

{-|
Run a Set Parameter command.
-}
doSetParameter :: Socket
               -> Equipment
               -> Action
               -> Word16
               -> NecControlIO Word16
doSetParameter monitor target action value = do
    let command = mkPacket target
                           Controller
                           (MsgSetParameter (actOpCode action) value)

    send monitor (necPack command)
    bin <- necRecv monitor
    case necUnpack bin of
         Left msg -> throwError $ msg ++ "\n" ++ prettyHex bin
         Right packet -> return $ repValue (cmpMessage packet)

{-|
The neccontrol main function.
-}
main :: IO ()
main = do
    args <- getArgs
    result <- runExceptT (parseArgs args)
    case result of
         Left msg -> hPutStrLn stderr msg >> exitWith (ExitFailure 1)
         _ -> exitSuccess
