module Network.NecControl.Commands.TemperatureSensor
(temperatureSensorCommands)
where

import Network.NecControl.Commands.Types

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

temperatureSensorCommands :: Category
temperatureSensorCommands = Category
    "Temperature sensor"
    [ selectTemperatureSensor, readoutTemperature ]
