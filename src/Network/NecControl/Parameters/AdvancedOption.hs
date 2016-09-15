{- |
Module      : AdvancedOption
Description : Commands of the advanced option category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the advanced option category.
-}
module Network.NecControl.Parameters.AdvancedOption
(advancedOptionCommands)
where

import Network.NecControl.Parameters.Types

inputDetect :: Action
inputDetect = Action
    { actName = "Input detect"
    , actCommand = "input-detect"
    , actOpCode = OpCode 0x0240
    , actConstraints = Values
        [ Value "first-detect" 0 ""
        , Value "last-detect" 1 ""
        , Value "none" 2 ""
        , Value "video-detect" 3 ""
        , Value "custom-detect" 4 ""
        ]
    }

customDetectPriority1 :: Action
customDetectPriority1 = Action
    { actName = "Custom detect (priority 1)"
    , actCommand = "custom-detect-1"
    , actOpCode = OpCode 0x102e
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

customDetectPriority2 :: Action
customDetectPriority2 = Action
    { actName = "Custom detect (priority 2)"
    , actCommand = "custom-detect-2"
    , actOpCode = OpCode 0x102f
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

customDetectPriority3 :: Action
customDetectPriority3 = Action
    { actName = "Custom detect (priority 3)"
    , actCommand = "custom-detect-3"
    , actOpCode = OpCode 0x1030
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

customDetectPriority4 :: Action
customDetectPriority4 = Action
    { actName = "Custom detect (priority 4)"
    , actCommand = "custom-detect-4"
    , actOpCode = OpCode 0x1031
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

customDetectPriority5 :: Action
customDetectPriority5 = Action
    { actName = "Custom detect (priority 5)"
    , actCommand = "custom-detect-5"
    , actOpCode = OpCode 0x1032
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

inputChange :: Action
inputChange = Action
    { actName = "Input change"
    , actCommand = "input-change"
    , actOpCode = OpCode 0x1086
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "normal" 1 ""
        , Value "quick" 2 ""
        ]
    }

dviMode :: Action
dviMode = Action
    { actName = "DVI mode (terminal setting)"
    , actCommand = "dvi-mode"
    , actOpCode = OpCode 0x02cf
    , actConstraints = Values
        [ Value "pc" 1 ""
        , Value "hd" 2 ""
        ]
    }

bncMode :: Action
bncMode = Action
    { actName = "BNC mode (terminal setting)"
    , actCommand = "bnc-mode"
    , actOpCode = OpCode 0x107e
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "rgb" 1 ""
        , Value "component" 2 ""
        , Value "video" 3 ""
        , Value "scart" 4 ""
        , Value "s-video" 4 ""
        ]
    }

dsubMode :: Action
dsubMode = Action
    { actName = "D-sub mode (terminal setting)"
    , actCommand = "dsub-mode"
    , actOpCode = OpCode 0x108e
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "rgb" 1 ""
        , Value "component" 2 ""
        , Value "video" 3 ""
        , Value "scart" 4 ""
        , Value "s-video" 4 ""
        ]
    }

hdmiSignal :: Action
hdmiSignal = Action
    { actName = "HDMI signal (terminal setting)"
    , actCommand = "hdmi-signal"
    , actOpCode = OpCode 0x1040
    , actConstraints = Values
        [ Value "expand" 1 ""
        , Value "raw" 2 ""
        ]
    }

deinterlace :: Action
deinterlace = Action
    { actName = "Deinterlace"
    , actCommand = "deinterlace"
    , actOpCode = OpCode 0x0225
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "enable" 2 ""
        ]
    }

colorSystem :: Action
colorSystem = Action
    { actName = "Color system"
    , actCommand = "color-system"
    , actOpCode = OpCode 0x0221
    , actConstraints = Values
        [ Value "ntsc" 1 ""
        , Value "pal" 2 ""
        , Value "secam" 3 ""
        , Value "auto" 4 ""
        , Value "ntsc-443" 5 "4.43NTSC"
        , Value "pal-60" 6 "PAL-60"
        ]
    }

overScan :: Action
overScan = Action
    { actName = "Over scan"
    , actCommand = "over-scan"
    , actOpCode = OpCode 0x02e3
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "on" 2 ""
        ]
    }

optionSettingAudio :: Action
optionSettingAudio = Action
    { actName = "Option setting audio"
    , actCommand = "option-setting-audio"
    , actOpCode = OpCode 0x10b0
    , actConstraints = Values
        [ Value "analog" 1 ""
        , Value "digital" 2 ""
        ]
    }

motionCompensation :: Action
motionCompensation = Action
    { actName = "Motion compensation (120Hz)"
    , actCommand = "motion-compensation"
    , actOpCode = OpCode 0x1087
    , actConstraints = Values
        [ Value "on" 1 ""
        , Value "off" 2 ""
        ]
    }

{-|
Commands of the advanced option category: inputDetect, customDetectPriority1,
customDetectPriority2, customDetectPriority3, customDetectPriority4,
customDetectPriority5, inputChange, dviMode, bncMode, dsubMode, hdmiSignal,
deinterlace, colorSystem, overScan, optionSettingAudio, motionCompensation.
-}
advancedOptionCommands :: Category
advancedOptionCommands = Category
    "Advanced options"
    [ inputDetect, customDetectPriority1, customDetectPriority2
    , customDetectPriority3, customDetectPriority4, customDetectPriority5
    , inputChange, dviMode, bncMode, dsubMode, hdmiSignal, deinterlace
    , colorSystem, overScan, optionSettingAudio, motionCompensation
    ]
