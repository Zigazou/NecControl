module Network.NecControl.Commands.Schedule (scheduleCommands) where

import Network.NecControl.Commands.Types

offTimer :: Action
offTimer = Action
    { actName = "Off timer"
    , actCommand = "off-timer"
    , actOpCode = OpCode 0x022b
    , actConstraints = MinMax 0 24 "0=off, 1=1 hour, 24=24 hours"
    }

enableSchedule :: Action
enableSchedule = Action
    { actName = "Enable schedule"
    , actCommand = "enable-schedule"
    , actOpCode = OpCode 0x02e5
    , actConstraints = MinMax 1 7 "1=enable no.1, 7=enable no.7"
    }

disableSchedule :: Action
disableSchedule = Action
    { actName = "Disable schedule"
    , actCommand = "disable-schedule"
    , actOpCode = OpCode 0x02e6
    , actConstraints = MinMax 1 7 "1=disable no.1, 7=disable no.7"
    }

scheduleCommands :: Category
scheduleCommands = Category
    "Scheduling"
    [ offTimer, enableSchedule, disableSchedule ]
