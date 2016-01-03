{- |
Module      : Adjust
Description : Commands of the adjust category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the adjust category.
-}
module Network.NecControl.Commands.Adjust (adjustCommands) where

import Network.NecControl.Commands.Types

autoSetup :: Action
autoSetup = Action
    { actName = "Auto setup"
    , actCommand = "auto-setup"
    , actOpCode = OpCode 0x001e
    , actConstraints = Values [ Value "execute" 1 "Momentary" ]
    }

hPosition :: Action
hPosition = Action
    { actName = "H position"
    , actCommand = "h-position"
    , actOpCode = OpCode 0x0020
    , actConstraints =
        MinMax 0 65535 "0=left side, max=right side (depends on display timing)"
    }

vPosition :: Action
vPosition = Action
    { actName = "V position"
    , actCommand = "v-position"
    , actOpCode = OpCode 0x0030
    , actConstraints =
        MinMax 0 65535 "0=bottom side, max=top side (depends on display timing)"
    }

clock :: Action
clock = Action
    { actName = "Clock"
    , actCommand = "clock"
    , actOpCode = OpCode 0x000e
    , actConstraints = MinMax 0 65535 "display timing"
    }

phase :: Action
phase = Action
    { actName = "Phase"
    , actCommand = "phase"
    , actOpCode = OpCode 0x003e
    , actConstraints = MinMax 0 65535 "display timing"
    }

hResolution :: Action
hResolution = Action
    { actName = "H resolution"
    , actCommand = "h-resolution"
    , actOpCode = OpCode 0x0250
    , actConstraints = MinMax 0 65535 "0=low, max=high (display timing)"
    }

vResolution :: Action
vResolution = Action
    { actName = "V resolution"
    , actCommand = "v-resolution"
    , actOpCode = OpCode 0x0251
    , actConstraints = MinMax 0 65535 "0=low, max=high (display timing)"
    }

inputResolution :: Action
inputResolution = Action
    { actName = "Input resolution"
    , actCommand = "input-resolution"
    , actOpCode = OpCode 0x02da
    , actConstraints = Values
        [ Value "item-1" 1 "always auto"
        , Value "item-2" 2 ""
        , Value "item-3" 3 ""
        , Value "item-4" 4 ""
        , Value "item-5" 5 ""
        ]
    }

aspect :: Action
aspect = Action
    { actName = "Aspect"
    , actCommand = "aspect"
    , actOpCode = OpCode 0x0270
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "normal" 1 ""
        , Value "full" 2 ""
        , Value "wide" 3 "A/V mode only"
        , Value "zoom" 4 ""
        , Value "dynamic" 6 "A/V mode only"
        , Value "off" 7 "dot by dot"
        ]
    }

zoom :: Action
zoom = Action
    { actName = "Zoom"
    , actCommand = "zoom"
    , actOpCode = OpCode 0x026f
    , actConstraints = MinMax 1 201 "1=100%, 2=101%, 201=300%"
    }

zoomHExpansion :: Action
zoomHExpansion = Action
    { actName = "Zoom H expansion"
    , actCommand = "zoom-h-expansion"
    , actOpCode = OpCode 0x026c
    , actConstraints = MinMax 1 201 "1=100%, 2=101%, 201=300%"
    }

zoomVExpansion :: Action
zoomVExpansion = Action
    { actName = "Zoom V expansion"
    , actCommand = "zoom-v-expansion"
    , actOpCode = OpCode 0x026d
    , actConstraints = MinMax 1 201 "1=100%, 2=101%, 201=300%"
    }

zoomHPosition :: Action
zoomHPosition = Action
    { actName = "Zoom H position"
    , actCommand = "zoom-h-position"
    , actOpCode = OpCode 0x02cc
    , actConstraints = MinMax 1 201 "1=100%, 2=101%, 201=300%"
    }

zoomVPosition :: Action
zoomVPosition = Action
    { actName = "Zoom V position"
    , actCommand = "zoom-v-position"
    , actOpCode = OpCode 0x02cd
    , actConstraints = MinMax 1 201 "1=100%, 2=101%, 201=300%"
    }

{-|
Commands of the adjust category: autoSetup, hPosition, vPosition, clock,
phase, hResolution, vResolution, inputResolution, aspect, zoom,
zoomHExpansion, zoomVExpansion, zoomHPosition, zoomVPosition
-}
adjustCommands :: Category
adjustCommands = Category
    "Adjust"
    [ autoSetup, hPosition, vPosition, clock, phase, hResolution, vResolution
    , inputResolution, aspect, zoom, zoomHExpansion, zoomVExpansion
    , zoomHPosition, zoomVPosition
    ]