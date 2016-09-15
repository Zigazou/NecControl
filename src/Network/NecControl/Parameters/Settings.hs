{- |
Module      : Settings
Description : Commands of the settings category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the settings category.
-}
module Network.NecControl.Parameters.Settings (settingsCommands) where

import Network.NecControl.Parameters.Types

input :: Action
input = Action
    { actName = "Input"
    , actCommand = "input"
    , actOpCode = OpCode 0x0060
    , actConstraints = Values
        [ Value "vga" 1 ""
        , Value "rgb-hv" 2 ""
        , Value "dvi" 3 ""
        , Value "hdmi" 4 ""
        , Value "video-1" 5 ""
        , Value "video-2" 6 ""
        , Value "s-video" 7 ""
        , Value "dvd-hd1" 12 ""
        , Value "dvd-hd2" 14 ""
        , Value "option" 13 ""
        , Value "display-port" 15 ""
        ]
    }

mute :: Action
mute = Action
    { actName = "Mute"
    , actCommand = "mute"
    , actOpCode = OpCode 0x008d
    , actConstraints = Values
        [ Value "unmute" 0 ""
        , Value "mute" 1 ""
        ]
    }

screenMute :: Action
screenMute = Action
    { actName = "Screen mute"
    , actCommand = "screen-mute"
    , actOpCode = OpCode 0x10b6
    , actConstraints = Values
        [ Value "on" 1 ""
        , Value "off" 2 ""
        ]
    }

mts :: Action
mts = Action
    { actName = "MTS"
    , actCommand = "mts"
    , actOpCode = OpCode 0x022c
    , actConstraints = Values
        [ Value "main" 1 ""
        , Value "sub" 2 ""
        , Value "main-sub" 2 "Main + sub"
        ]
    }

stillCapture :: Action
stillCapture = Action
    { actName = "Still capture (momentary)"
    , actCommand = "still-capture"
    , actOpCode = OpCode 0x0276
    , actConstraints = Values
        [ Value "off" 0 ""
        , Value "capture" 1 ""
        ]
    }

signalInformation :: Action
signalInformation = Action
    { actName = "Signal information"
    , actCommand = "signal-information"
    , actOpCode = OpCode 0x02ea
    , actConstraints = Values
        [ Value "no-action" 0 ""
        , Value "off" 1 "no indication"
        , Value "on" 2 "indication"
        ]
    }

tvChannel :: Action
tvChannel = Action
    { actName = "TV channel (up/down)"
    , actCommand = "tv-channel"
    , actOpCode = OpCode 0x008b
    , actConstraints = Values
        [ Value "no-action" 0 ""
        , Value "up" 1 ""
        , Value "down" 2 ""
        ]
    }

{-|
Commands of the settings category: input, mute, screenMute, mts, stillCapture,
signalInformation, tvChannel.
-}
settingsCommands :: Category
settingsCommands = Category
    "Settings"
    [ input, mute, screenMute, mts, stillCapture, signalInformation
    , tvChannel
    ]
