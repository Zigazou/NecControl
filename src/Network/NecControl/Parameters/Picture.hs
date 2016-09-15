{- |
Module      : Picture
Description : Commands of the picture category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the picture category.
-}
module Network.NecControl.Parameters.Picture (pictureCommands) where

import Network.NecControl.Parameters.Types

backlight :: Action
backlight = Action
    { actName = "Backlight"
    , actCommand = "backlight"
    , actOpCode = OpCode 0x0010
    , actConstraints = MinMax 0 100 "0=dark, 100=bright"
    }

contrast :: Action
contrast = Action
    { actName = "Contrast"
    , actCommand = "contrast"
    , actOpCode = OpCode 0x0012
    , actConstraints = MinMax 0 100 "0=low, 100=high"
    }

sharpness :: Action
sharpness = Action
    { actName = "Sharpness"
    , actCommand = "sharpness"
    , actOpCode = OpCode 0x008c
    , actConstraints = MinMax 0 24 "0=dull, 24=sharp"
    }

brightness :: Action
brightness = Action
    { actName = "Brightness"
    , actCommand = "brightness"
    , actOpCode = OpCode 0x0092
    , actConstraints = MinMax 0 100 "0=dark, 100=bright"
    }

hue :: Action
hue = Action
    { actName = "Hue"
    , actCommand = "hue"
    , actOpCode = OpCode 0x0090
    , actConstraints = MinMax 0 100 "0=purplish, 100=greenish"
    }

color :: Action
color = Action
    { actName = "Color"
    , actCommand = "color"
    , actOpCode = OpCode 0x021f
    , actConstraints = MinMax 0 100 "0=pale, 100=deep"
    }

colorTemperature :: Action
colorTemperature = Action
    { actName = "Color temperature"
    , actCommand = "color-temperature"
    , actOpCode = OpCode 0x0054
    , actConstraints = MinMax 0 74 "0=2600K, 74=10000K"
    }

colorRed :: Action
colorRed = Action
    { actName = "Color control red"
    , actCommand = "red"
    , actOpCode = OpCode 0x009b
    , actConstraints = MinMax 0 200 "0=min, 100=center, 200=max"
    }

colorYellow :: Action
colorYellow = Action
    { actName = "Color control yellow"
    , actCommand = "yellow"
    , actOpCode = OpCode 0x009c
    , actConstraints = MinMax 0 200 "0=min, 100=center, 200=max"
    }

colorGreen :: Action
colorGreen = Action
    { actName = "Color control green"
    , actCommand = "green"
    , actOpCode = OpCode 0x009d
    , actConstraints = MinMax 0 200 "0=min, 100=center, 200=max"
    }

colorCyan :: Action
colorCyan = Action
    { actName = "Color control cyan"
    , actCommand = "cyan"
    , actOpCode = OpCode 0x009e
    , actConstraints = MinMax 0 200 "0=min, 100=center, 200=max"
    }

colorBlue :: Action
colorBlue = Action
    { actName = "Color control blue"
    , actCommand = "blue"
    , actOpCode = OpCode 0x009f
    , actConstraints = MinMax 0 200 "0=min, 100=center, 200=max"
    }

colorMagenta :: Action
colorMagenta = Action
    { actName = "Color control magenta"
    , actCommand = "magenta"
    , actOpCode = OpCode 0x00a0
    , actConstraints = MinMax 0 200 "0=min, 100=center, 200=max"
    }

gammaCorrection :: Action
gammaCorrection = Action
    { actName = "Gamma correction"
    , actCommand = "gamma"
    , actOpCode = OpCode 0x0268
    , actConstraints = Values
        [ Value "native" 1 "Native gamma"
        , Value "2.2" 4 "Gamma 2.2"
        , Value "2.4" 8 "Gamma 2.4"
        , Value "s" 7 "S Gamma"
        , Value "dicom-sim" 5 "DICOM SIM"
        , Value "programmable" 6 "Programmable"
        ]
    }

adaptiveContrast :: Action
adaptiveContrast = Action
    { actName = "Adaptive contrast (movie settings)"
    , actCommand = "adaptive-contrast"
    , actOpCode = OpCode 0x028d
    , actConstraints = Values
        [ Value "none" 0 ""
        , Value "off" 1 ""
        , Value "low" 2 ""
        , Value "middle" 3 ""
        , Value "high" 4 ""
        ]
    }

noiseReduction :: Action
noiseReduction = Action
    { actName = "Noise reduction (movie settings)"
    , actCommand = "noise-reduction"
    , actOpCode = OpCode 0x0226
    , actConstraints = Values
        [ Value "off" 0 ""
        , Value "lowest" 1 ""
        , Value "low" 2 ""
        , Value "middle" 3 ""
        , Value "high" 4 ""
        , Value "highest" 5 ""
        ]
    }

telecine :: Action
telecine = Action
    { actName = "Telecine (movie settings)"
    , actCommand = "telecine"
    , actOpCode =  OpCode 0x0223
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "auto" 2 ""
        ]
    }

pictureMode :: Action
pictureMode = Action
    { actName = "Picture mode"
    , actCommand = "picture-mode"
    , actOpCode = OpCode 0x021a
    , actConstraints = Values
        [ Value "srgb" 1 "PC mode only"
        , Value "hi-bright" 3 ""
        , Value "standard" 4 ""
        , Value "cinema" 5 "A/V mode only"
        , Value "isf-day" 6 "needs an adjustment by ISF"
        , Value "isf-night" 7 "needs an adjustment by ISF"
        , Value "ambient-1" 11 ""
        , Value "ambient-2" 12 ""
        ]
    }

ambientBrightnessLow :: Action
ambientBrightnessLow = Action
    { actName = "Ambient brightness low (ambient)"
    , actCommand = "amb-brightness-low"
    , actOpCode = OpCode 0x1033
    , actConstraints = MinMax 0 100 "0=dark, 100=bright"
    }

ambientBrightnessHigh :: Action
ambientBrightnessHigh = Action
    { actName = "Ambient brightness high (ambient)"
    , actCommand = "amb-brightness-high"
    , actOpCode = OpCode 0x1034
    , actConstraints = MinMax 0 100 "0=dark, 100=bright"
    }

getCurrentIlluminance :: Action
getCurrentIlluminance = Action
    { actName = "Current illuminance (ambient)"
    , actCommand = "illuminance"
    , actOpCode =  OpCode 0x02b4
    , actConstraints = ReadOnly
    }

brightSensorRead :: Action
brightSensorRead = Action
    { actName = "Bright sensor read (ambient)"
    , actCommand = "bright-sensor-read"
    , actOpCode = OpCode 0x02b5
    , actConstraints = ReadOnly
    }


{-|
Commands of the picture category: backlight, contrast, sharpness, brightness,
hue, color, colorTemperature ,colorRed, colorYellow, colorGreen, colorCyan,
colorBlue, colorMagenta, gammaCorrection, adaptiveContrast, noiseReduction,
telecine, pictureMode, ambientBrightnessLow, ambientBrightnessHigh,
getCurrentIlluminance, brightSensorRead.
-}
pictureCommands :: Category
pictureCommands = Category
    "Picture"
    [ backlight, contrast, sharpness, brightness, hue, color, colorTemperature
    , colorRed, colorYellow, colorGreen, colorCyan, colorBlue, colorMagenta
    , gammaCorrection, adaptiveContrast, noiseReduction, telecine, pictureMode
    , ambientBrightnessLow, ambientBrightnessHigh, getCurrentIlluminance
    , brightSensorRead
    ]
