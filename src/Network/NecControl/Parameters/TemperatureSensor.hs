{- |
Module      : Temperature
Description : Commands of the temperature category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the temperature category.
-}
module Network.NecControl.Parameters.TemperatureSensor
(temperatureSensorCommands)
where

import Network.NecControl.Parameters.Types

selectTemperatureSensor :: Action
selectTemperatureSensor = Action
    { actName = "Select temperature sensor"
    , actCommand = "select-temperature-sensor"
    , actOpCode = OpCode 0x0278
    , actConstraints = MinMax 1 3 "1=sensor #1, 3=sensor #3"
    }

readoutTemperature :: Action
readoutTemperature = Action
    { actName = "Readout a temperature"
    , actCommand = "temperature"
    , actOpCode = OpCode 0x0279
    , actConstraints = ReadOnly
    }

{-|
Commands of the temperature category: selectTemperatureSensor,
readoutTemperature.
-}
temperatureSensorCommands :: Category
temperatureSensorCommands = Category
    "Temperature sensor"
    [ selectTemperatureSensor, readoutTemperature ]
