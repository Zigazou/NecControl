{- |
Module      : SelfDiagnosis
Description : Handle the self diagnosis command and reply
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Handle the self diagnosis command and reply.
-}
module Network.NecControl.Commands.SelfDiagnosis
( 
)
where

import Data.Word (Word8, Word16)
import Control.Monad (when)

import Network.NecControl.NecProtocol (NecValue, toNec, fromNec)
import Network.NecControl.PacketStructure
    (PacketStructure (StartOfMessage, EndOfMessage))
import Network.NecControl.Commands.Types (msgConcat, ResultCode)

data PowerStatusMode = PsmOn
                     | PsmStandBy
                     | PsmSuspend
                     | PsmOff
                     deriving (Eq, Show)

instance NecValue PowerStatusMode where
    toNec PsmOn = toNec (0x0001 :: Word16)
    toNec PsmStandBy = toNec (0x0002 :: Word16)
    toNec PsmSuspend = toNec (0x0003 :: Word16)
    toNec PsmOff = toNec (0x0004 :: Word16)

    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmOn
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmStandBy
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmSuspend
    fromNec [0x30, 0x30, 0x30, 0x31] = Right PsmOff
    fromNec _ = Left "Invalid power status mode"

{-|
-}
data SelfDiagnosis = SelfDiagnosis deriving (Eq, Show)

instance NecValue SelfDiagnosis where
    toNec PowerStatus = msgConcat [toNec (0xb1 :: Word8)]

    fromNec _ = error "Self diagnosis from monitor is unsupported"

data SelfTestResult = SelfTestNormal
                    | StandByPower33vAbnormality
                    | StandByPower5vAbnormality
                    | PanelPower12vAbnormality
                    | InverterPower24vAbnormality
                    | CoolingFan1Abnormality
                    | CoolingFan2Abnormality
                    | CoolingFan3Abnormality
                    | InverterAbnormality
                    | LedBacklightAbnormality
                    | TemperatureShutdownAbnormality
                    | TemperatureHalfBrightnessAbnormality
                    | SensorReachedUserTemperature
                    | NoSignal
                    | OptionBoardAbnormality
                    deriving (Eq, Show)

instance NecValue SelfTestResult where
    toNec SelfTestNormal = [0x30, 0x30]
    toNec StandByPower33vAbnormality = [0x37, 0x30]
    toNec StandByPower5vAbnormality = [0x37, 0x31]
    toNec PanelPower12vAbnormality = [0x37, 0x32]
    toNec InverterPower24vAbnormality = [0x37, 0x38]
    toNec CoolingFan1Abnormality = [0x38, 0x30]
    toNec CoolingFan2Abnormality = [0x38, 0x31]
    toNec CoolingFan3Abnormality = [0x38, 0x32]
    toNec InverterAbnormality = [0x39, 0x30]
    toNec LedBacklightAbnormality = [0x39, 0x31]
    toNec TemperatureShutdownAbnormality = [0x41, 0x30]
    toNec TemperatureHalfBrightnessAbnormality = [0x41, 0x31]
    toNec SensorReachedUserTemperature = [0x41, 0x32]
    toNec NoSignal = [0x42, 0x30]
    toNec OptionBoardAbnormality = [0x43, 0x30]

    fromNec [0x30, 0x30] = Right SelfTestNormal
    fromNec [0x37, 0x30] = Right StandByPower33vAbnormality
    fromNec [0x37, 0x31] = Right StandByPower5vAbnormality
    fromNec [0x37, 0x32] = Right PanelPower12vAbnormality
    fromNec [0x37, 0x38] = Right InverterPower24vAbnormality
    fromNec [0x38, 0x30] = Right CoolingFan1Abnormality
    fromNec [0x38, 0x31] = Right CoolingFan2Abnormality
    fromNec [0x38, 0x32] = Right CoolingFan3Abnormality
    fromNec [0x39, 0x30] = Right InverterAbnormality
    fromNec [0x39, 0x31] = Right LedBacklightAbnormality
    fromNec [0x41, 0x30] = Right TemperatureShutdownAbnormality
    fromNec [0x41, 0x31] = Right TemperatureHalfBrightnessAbnormality
    fromNec [0x41, 0x32] = Right SensorReachedUserTemperature
    fromNec [0x42, 0x30] = Right NoSignal
    fromNec [0x43, 0x30] = Right OptionBoardAbnormality
    fromNec _ = Left "Invalid self test result"

data SelfDiagnosisReply = SelfDiagnosisReply
    { sdrResults :: [SelfTestResult] }
    deriving (Eq, Show)

instance NecValue SelfDiagnosisReply where
    toNec _ = error "Self diagnosis reply from controller is unsupported"

    fromNec (stx:0x41:0x31:rts) = do
        som <- fromNec [stx]
        eom <- fromNec $ last rts
        when (som /= StartOfMessage) (Left "Invalid start of message")
        when (eom /= EndOfMessage) (Left "Invalid end of message")
        results <- getResults $ init rts

        return $ SelfDiagnosisReply results
    where getResults (t1:t2:rts) = do
              test <- fromNec [t1, t2]
              tests <- getTest rts
              return (test:tests)
          getResults [] = return []
          getResults _ = Left "Invalid results"

    fromNec _ = Left "Invalid power status reply"
