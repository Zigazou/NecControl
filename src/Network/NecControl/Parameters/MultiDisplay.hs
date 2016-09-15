{- |
Module      : MultiDisplay
Description : Commands of the multi display category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the multi display category.
-}
module Network.NecControl.Parameters.MultiDisplay (multiDisplayCommands) where

import Network.NecControl.Parameters.Types

monitorId :: Action
monitorId = Action
    { actName = "Monitor ID"
    , actCommand = "monitor-id"
    , actOpCode = OpCode 0x023e
    , actConstraints = MinMax 1 100 ""
    }

groupId :: Action
groupId = Action
    { actName = "Group ID"
    , actCommand = "group-id"
    , actOpCode = OpCode 0x107f
    , actConstraints = MinMax 0 1023 "bit 0=Group A, bit 9=Group J"
    }

irControl :: Action
irControl = Action
    { actName = "IR control"
    , actCommand = "ir-control"
    , actOpCode = OpCode 0x023f
    , actConstraints = Values
        [ Value "normal" 1 ""
        , Value "primary" 2 ""
        , Value "secondary" 3 ""
        , Value "lock" 4 "off"
        ]
    }

tileMatrixHMonitor :: Action
tileMatrixHMonitor = Action
    { actName = "H monitor (tile matrix)"
    , actCommand = "tile-matrix-h-monitor"
    , actOpCode = OpCode 0x02d0
    , actConstraints = MinMax 1 10 "Number of H-division"
    }

tileMatrixVMonitor :: Action
tileMatrixVMonitor = Action
    { actName = "V monitor (tile matrix)"
    , actCommand = "tile-matrix-v-monitor"
    , actOpCode = OpCode 0x02d1
    , actConstraints = MinMax 1 10 "Number of V-division"
    }

tileMatrixPosition :: Action
tileMatrixPosition = Action
    { actName = "Position (tile matrix)"
    , actCommand = "tile-matrix-position"
    , actOpCode = OpCode 0x02d2
    , actConstraints = MinMax 1 100 "1=upper left, max=lower right"
    }

tileComp :: Action
tileComp = Action
    { actName = "Tile comp (tile matrix)"
    , actCommand = "tile-comp"
    , actOpCode = OpCode 0x02d5
    , actConstraints = Values
        [ Value "disable" 1 "off"
        , Value "enable" 2 "on"
        ]
    }

tileMatrixMode :: Action
tileMatrixMode = Action
    { actName = "Mode (tile matrix)"
    , actCommand = "tile-matrix-mode"
    , actOpCode = OpCode 0x02d3
    , actConstraints = Values
        [ Value "disable-display" 1 "off and display frame"
        , Value "disable-erase" 3 "off and erase frame"
        , Value "enable" 2 "on"
        ]
    }

tileMatrixMem :: Action
tileMatrixMem = Action
    { actName = "Mem (tile matrix)"
    , actCommand = "tile-matrix-mem"
    , actOpCode = OpCode 0x104a
    , actConstraints = Values
        [ Value "none" 0 ""
        , Value "common" 1 "default"
        , Value "each-input" 2 ""
        ]
    }

powerOnDelay :: Action
powerOnDelay = Action
    { actName = "Power on delay"
    , actCommand = "power-on-delay"
    , actOpCode = OpCode 0x02d8
    , actConstraints = MinMax 0 50 "0=off (0s), 50=50s"
    }

powerIndicator :: Action
powerIndicator = Action
    { actName = "Power indicator"
    , actCommand = "power-indicator"
    , actOpCode = OpCode 0x02be
    , actConstraints = Values
        [ Value "on" 1 ""
        , Value "off" 2 ""
        ]
    }

externalControl :: Action
externalControl = Action
    { actName = "External control"
    , actCommand = "external-control"
    , actOpCode = OpCode 0x103e
    , actConstraints = Values
        [ Value "rs232c" 1 ""
        , Value "lan" 2 ""
        ]
    }

{-|
Commands of the multi display category: monitorId, groupId, irControl,
tileMatrixHMonitor, tileMatrixVMonitor, tileMatrixPosition, tileComp,
tileMatrixMode, tileMatrixMem, powerOnDelay, powerIndicator, externalControl.
-}
multiDisplayCommands :: Category
multiDisplayCommands = Category
    "Multi display"
    [ monitorId, groupId, irControl, tileMatrixHMonitor, tileMatrixVMonitor
    , tileMatrixPosition, tileComp, tileMatrixMode, tileMatrixMem, powerOnDelay
    , powerIndicator, externalControl
    ]
