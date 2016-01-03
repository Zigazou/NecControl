{- |
Module      : Osd
Description : Commands of the OSD (On-Screen Display) category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the OSD (On-Screen Display) category.
-}
module Network.NecControl.Commands.Osd (osdCommands) where

import Network.NecControl.Commands.Types

language :: Action
language = Action
    { actName = "Language (OSD)"
    , actCommand = "language"
    , actOpCode = OpCode 0x0068
    , actConstraints = Values
        [ Value "english" 1 ""
        , Value "german" 2 ""
        , Value "french" 3 ""
        , Value "spanish" 4 ""
        , Value "japanese" 5 ""
        , Value "italian" 6 ""
        , Value "swedish" 7 ""
        , Value "russian" 9 ""
        , Value "chinese" 14 ""
        ]
    }

menuDisplayTime :: Action
menuDisplayTime = Action
    { actName = "Menu display time"
    , actCommand = "menu-display-time"
    , actOpCode = OpCode 0x00fc
    , actConstraints = MinMax 2 48 "2=10s, 3=15s, 48=240s (5 sec/step)"
    }

osdHPosition :: Action
osdHPosition = Action
    { actName = "OSD H position"
    , actCommand = "osd-h-position"
    , actOpCode = OpCode 0x0238
    , actConstraints = MinMax 0 8192 "0=left, max=right"
    }

osdVPosition :: Action
osdVPosition = Action
    { actName = "OSD V position"
    , actCommand = "osd-v-position"
    , actOpCode = OpCode 0x0239
    , actConstraints = MinMax 0 8192 "0=down, max=up"
    }

informationOsd :: Action
informationOsd = Action
    { actName = "Information OSD"
    , actCommand = "osd"
    , actOpCode = OpCode 0x023d
    , actConstraints = Values
        [ Value "disable" 0 "disable information OSD"
        , Value "3s" 3 ""
        , Value "4s" 4 ""
        , Value "5s" 5 ""
        , Value "6s" 6 ""
        , Value "7s" 7 ""
        , Value "8s" 8 ""
        , Value "9s" 9 ""
        , Value "10s" 10 ""
        ]
    }

osdTransparency :: Action
osdTransparency = Action
    { actName = "OSD transparency"
    , actCommand = "osd-transparency"
    , actOpCode = OpCode 0x02b8
    , actConstraints = Values
        [ Value "opaque" 1 ""
        , Value "translucent" 2 ""
        ]
    }

osdRotation :: Action
osdRotation = Action
    { actName = "OSD rotation"
    , actCommand = "osd-rotation"
    , actOpCode = OpCode 0x0241
    , actConstraints = Values
        [ Value "normal" 0 ""
        , Value "rotated" 1 ""
        ]
    }

closedCaption :: Action
closedCaption = Action
    { actName = "Closed caption"
    , actCommand = "closed-caption"
    , actOpCode = OpCode 0x1084
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "cc1" 2 ""
        , Value "cc2" 3 ""
        , Value "cc3" 4 ""
        , Value "cc4" 5 ""
        , Value "tt1" 6 ""
        , Value "tt2" 7 ""
        , Value "tt3" 8 ""
        , Value "tt4" 9 ""
        ]
    }

{-|
Commands of the OSD (On-Screen Display) category: language, menuDisplayTime,
osdHPosition, osdVPosition, informationOsd, osdTransparency, osdRotation,
closedCaption.
-}
osdCommands :: Category
osdCommands = Category
    "On-Screen Display (OSD)"
    [ language, menuDisplayTime, osdHPosition, osdVPosition, informationOsd
    , osdTransparency, osdRotation, closedCaption
    ]
