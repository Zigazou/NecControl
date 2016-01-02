module Network.NecControl.Commands.Pip (pipCommands) where

import Network.NecControl.Commands.Types

keepPipMode :: Action
keepPipMode = Action
    { actName = "Keep PIP mode"
    , actCommand = "keep-pip-mode"
    , actOpCode = OpCode 0x0272
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "pip" 2 ""
        , Value "pop" 3 ""
        , Value "still" 4 ""
        , Value "aspect" 5 "side by side"
        , Value "full" 6 "side by side"
        ]
    }

pipSize :: Action
pipSize = Action
    { actName = "PIP size"
    , actCommand = "pip-size"
    , actOpCode = OpCode 0x0271
    , actConstraints = Values
        [ Value "small" 1 ""
        , Value "middle" 2 ""
        , Value "large" 3 ""
        ]
    }

pipHPosition :: Action
pipHPosition = Action
    { actName = "PIP H position"
    , actCommand = "pip-h-position"
    , actOpCode = OpCode 0x0274
    , actConstraints = MinMax 0 100 "0=left, 100=right"
    }

pipVPosition :: Action
pipVPosition = Action
    { actName = "PIP V position"
    , actCommand = "pip-v-position"
    , actOpCode = OpCode 0x0275
    , actConstraints = MinMax 0 100 "0=top, 100=bottom"
    }

pipAspect :: Action
pipAspect = Action
    { actName = "PIP aspect"
    , actCommand = "pip-aspect"
    , actOpCode = OpCode 0x1083
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "normal" 1 ""
        , Value "full" 2 ""
        , Value "wide" 3 ""
        ]
    }

textTickerMode :: Action
textTickerMode = Action
    { actName = "Mode (text ticker)"
    , actCommand = "text-ticker-mode"
    , actOpCode = OpCode 0x1008
    , actConstraints = Values
        [ Value "none" 0 ""
        , Value "off" 1 ""
        , Value "horizontal" 2 ""
        , Value "vertical" 3 ""
        ]
    }

textTickerPosition :: Action
textTickerPosition = Action
    { actName = "Position (text ticker)"
    , actCommand = "text-ticker-position"
    , actOpCode = OpCode 0x1009
    , actConstraints = MinMax 0 100 "0=top/left, 100=bottom/right"
    }

textTickerSize :: Action
textTickerSize = Action
    { actName = "Size (text ticker)"
    , actCommand = "text-ticker-size"
    , actOpCode = OpCode 0x100a
    , actConstraints = MinMax 2 8 "2=narrow(2/24), 8=wide(8/24)"
    }

textTickerBlend :: Action
textTickerBlend = Action
    { actName = "Blend (text ticker)"
    , actCommand = "text-ticker-blend"
    , actOpCode = OpCode 0x100b
    , actConstraints = MinMax 1 10 "1=10%, 10=100%"
    }

textTickerDetect :: Action
textTickerDetect = Action
    { actName = "Detect (text ticker)"
    , actCommand = "text-ticker-detect"
    , actOpCode = OpCode 0x100c
    , actConstraints = Values
        [ Value "on" 1 ""
        , Value "off" 2 ""
        ]
    }

textTickerFadeIn :: Action
textTickerFadeIn = Action
    { actName = "Fade in (text ticker)"
    , actCommand = "text-ticker-fade-in"
    , actOpCode = OpCode 0x100d
    , actConstraints = Values
        [ Value "on" 1 ""
        , Value "off" 2 ""
        ]
    }

pipInput :: Action
pipInput = Action
    { actName = "PIP input (sub input)"
    , actCommand = "pip-input"
    , actOpCode = OpCode 0x0273
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

pipCommands :: Category
pipCommands = Category
    "Picture-In-Picture (PIP)"
    [ keepPipMode, pipSize, pipHPosition, pipVPosition, pipAspect
    , textTickerMode, textTickerPosition, textTickerSize, textTickerBlend
    , textTickerDetect, textTickerFadeIn, pipInput
    ]
