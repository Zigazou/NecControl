module Main where

import Data.List (find)
import System.Environment (getArgs)
import Network.Simple.TCP
import Control.Concurrent
import Data.Word (Word16)
import Data.Char (ord)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))

import Hexdump (prettyHex)

import Network.NecControl.Commands.Types
import Network.NecControl.Commands.All (allCommands)
import Network.NecControl.Header
import Network.NecControl.NecProtocol
import Network.NecControl.CommandPacket
import Network.NecControl.Message

import Control.Monad.Except

type NecControlIO = ExceptT String IO

ncPutStrLn :: String -> ExceptT String IO ()
ncPutStrLn = liftIO . putStrLn

getActions :: [Category] -> [Action]
getActions = concatMap catActions

findActionByCmd :: String -> [Action] -> Either String Action
findActionByCmd str actions =
    case find (\a -> str == actCommand a) actions of
         Nothing -> Left "Unknown command"
         Just action -> Right action

liftE :: Either String a -> NecControlIO a
liftE e = case e of
               Left msg -> throwError msg
               Right v -> return v

liftMaybe :: String -> Maybe a -> NecControlIO a
liftMaybe msg mba = case mba of
                         Nothing -> throwError msg
                         Just v -> return v

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

allCommandHelp :: String
allCommandHelp = unlines $ "List of available commands:" : pretty allCommands

parseArgs :: [String] -> NecControlIO ()
parseArgs [ "help" ] = ncPutStrLn $ unlines [simpleHelp, "", allCommandHelp]

parseArgs [ "help", command ] = do
    action <- liftE $ findActionByCmd command (getActions allCommands)
    ncPutStrLn $ unlines $ pretty action

parseArgs (ipAddress:[monitorId]:todo) = do
    equipment <- liftE $ fromNec [(fromIntegral . ord) monitorId]
    execute ipAddress equipment todo

parseArgs _ = ncPutStrLn simpleHelp

execute :: String -> Equipment -> [String] -> NecControlIO ()
execute _ _ [] = return ()

execute host target ("get":command:_) = do
    action <- liftE $ findActionByCmd command (getActions allCommands)
    (monitor, _) <- liftIO $ connectSock host "7142"
    value <- doGetParameter monitor target action
    ncPutStrLn $ fromValue action value
    liftIO $ closeSock monitor

execute host target ("set":command:value:_) = do
    action <- liftE $ findActionByCmd command (getActions allCommands)
    value' <- liftE $ toValue action value
    (monitor, _) <- liftIO $ connectSock host "7142"
    newValue <- doSetParameter monitor target action value'
    ncPutStrLn $ fromValue action newValue
    liftIO $ closeSock monitor

execute _ _ _ = ncPutStrLn simpleHelp >> throwError "Invalid command"

doGetParameter :: Socket -> Equipment -> Action -> NecControlIO Word16
doGetParameter monitor target action = do
    let command = mkPacket target
                           Controller
                           (MsgGetParameter $ actOpCode action)

    send monitor (necPack command)
    liftIO $ threadDelay 100000
    reply <- recv monitor 64
    bin <- liftMaybe "Monitor did not reply" reply

    case necUnpack bin of
         Left msg -> throwError $ msg ++ "\n" ++ prettyHex bin
         Right packet -> return $ repValue (cmpMessage packet)

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
    liftIO $ threadDelay 800000
    reply <- recv monitor 64
    bin <- liftMaybe "Monitor did not reply" reply
    case necUnpack bin of
         Left msg -> throwError $ msg ++ "\n" ++ prettyHex bin
         Right packet -> return $ repValue (cmpMessage packet)

main :: IO ()
main = do
    args <- getArgs
    result <- runExceptT (parseArgs args)
    case result of
         Left msg -> hPutStrLn stderr msg >> exitWith (ExitFailure 1)
         _ -> exitSuccess
