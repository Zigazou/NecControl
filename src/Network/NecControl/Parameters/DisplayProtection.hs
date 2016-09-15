{- |
Module      : DisplayProtection
Description : Commands of the display protection category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the display protection category.
-}
module Network.NecControl.Parameters.DisplayProtection
(displayProtectionCommands)
where

import Network.NecControl.Parameters.Types

powerSave :: Action
powerSave = Action
    { actName = "Power save"
    , actCommand = "power-save"
    , actOpCode = OpCode 0x00e1
    , actConstraints = Values
        [ Value "off" 0 ""
        , Value "on" 1 ""
        ]
    }

videoPowerSave :: Action
videoPowerSave = Action
    { actName = "Video power save"
    , actCommand = "video-power-save"
    , actOpCode = OpCode 0x02d6
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "on" 2 ""
        ]
    }

fanControl :: Action
fanControl = Action
    { actName = "Fan control"
    , actCommand = "fan-control"
    , actOpCode = OpCode 0x027d
    , actConstraints = Values
        [ Value "none" 0 ""
        , Value "auto" 1 ""
        , Value "forced-on" 2 ""
        , Value "auto-2" 3 "offset -2"
        , Value "auto-4" 4 "offset -4"
        , Value "auto-6" 5 "offset -6"
        , Value "auto-8" 6 "offset -8"
        , Value "auto-10" 7 "offset -10"
        ]
    }

fanSpeed :: Action
fanSpeed = Action
    { actName = "Fan speed"
    , actCommand = "fan-speed"
    , actOpCode = OpCode 0x103f
    , actConstraints = Values
        [ Value "none" 0 ""
        , Value "high" 1 ""
        , Value "low" 2 ""
        ]
    }

screenSaverGamma :: Action
screenSaverGamma = Action
    { actName = "Gamma (screen saver)"
    , actCommand = "screen-saver-gamma"
    , actOpCode = OpCode 0x02db
    , actConstraints = Values
        [ Value "normal" 1 ""
        , Value "screen-saving-gamma" 2 ""
        ]
    }

screenSaverBrightness :: Action
screenSaverBrightness = Action
    { actName = "Brightness (screen saver)"
    , actCommand = "screen-saver-brightness"
    , actOpCode = OpCode 0x02dc
    , actConstraints = Values
        [ Value "normal" 1 ""
        , Value "decrease-brightness" 2 ""
        ]
    }

screenSaverMotion :: Action
screenSaverMotion = Action
    { actName = "Motion (screen saver)"
    , actCommand = "screen-saver-motion"
    , actOpCode = OpCode 0x02dd
    , actConstraints = MinMax 0 90 "0=off (0s), 90=900s (10s/step)"
    }

sideBorderColor :: Action
sideBorderColor = Action
    { actName = "Side border color"
    , actCommand = "side-border-color"
    , actOpCode = OpCode 0x02df
    , actConstraints = MinMax 0 100 "0=black, 100=white"
    }

autoBrightness :: Action
autoBrightness = Action
    { actName = "Auto brightness"
    , actCommand = "auto-brightness"
    , actOpCode = OpCode 0x022d
    , actConstraints = Values
        [ Value "off" 0 ""
        , Value "on" 1 ""
        ]
    }

alertMail :: Action
alertMail = Action
    { actName = "Alert mail"
    , actCommand = "alert-mail"
    , actOpCode = OpCode 0x108b
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "on" 2 ""
        ]
    }

{-|
Commands of the display protection category: powerSave, videoPowerSave,
fanControl, fanSpeed, screenSaverGamma, screenSaverBrightness,
screenSaverMotion, sideBorderColor, autoBrightness, alertMail.
-}
displayProtectionCommands :: Category
displayProtectionCommands = Category
    "Display protection"
    [ powerSave, videoPowerSave, fanControl, fanSpeed, screenSaverGamma
    , screenSaverBrightness, screenSaverMotion, sideBorderColor, autoBrightness
    , alertMail
    ]
