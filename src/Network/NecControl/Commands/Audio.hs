{- |
Module      : Audio
Description : Commands of the audio category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the audio category.
-}
module Network.NecControl.Commands.Audio (audioCommands) where

import Network.NecControl.Commands.Types

volume :: Action
volume = Action
    { actName = "Volume"
    , actCommand = "volume"
    , actOpCode = OpCode 0x0062
    , actConstraints = MinMax 0 100 "0=whisper, 100=loud"
    }

balance :: Action
balance = Action
    { actName = "Balance"
    , actCommand = "balance"
    , actOpCode = OpCode 0x0093
    , actConstraints = MinMax 0 60 "0=left, 30=center, 60=right"
    }

treble :: Action
treble = Action
    { actName = "Treble"
    , actCommand = "treble"
    , actOpCode = OpCode 0x008f
    , actConstraints = MinMax 0 12 "0=min, 6=center, 12=max"
    }

bass :: Action
bass = Action
    { actName = "Bass"
    , actCommand = "bass"
    , actOpCode = OpCode 0x0091
    , actConstraints = MinMax 0 12 "0=min, 6=center, 12=max"
    }

pipAudio :: Action
pipAudio = Action
    { actName = "PIP audio"
    , actCommand = "pip-audio"
    , actOpCode = OpCode 0x1080
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "main" 1 ""
        , Value "sub" 2 ""
        ]
    }

lineOut :: Action
lineOut = Action
    { actName = "Line out"
    , actCommand = "lineout"
    , actOpCode = OpCode 0x1081
    , actConstraints = Values
        [ Value "no-operate" 0 ""
        , Value "fixed" 1 ""
        , Value "variable" 2 ""
        ]
    }

surround :: Action
surround = Action
    { actName = "Surround"
    , actCommand = "surround"
    , actOpCode = OpCode 0x0234
    , actConstraints = Values
        [ Value "off" 1 ""
        , Value "low" 2 ""
        , Value "high" 3 ""
        ]
    }

audioInput :: Action
audioInput = Action
    { actName = "Audio input"
    , actCommand = "audio-input"
    , actOpCode = OpCode 0x022e
    , actConstraints = Values
        [ Value "audio-1" 1 "PC"
        , Value "audio-2" 2 ""
        , Value "audio-3" 3 ""
        , Value "hdmi" 4 ""
        , Value "tv-option" 6 ""
        , Value "display-port" 7 ""
        ]
    }

{-|
Commands of the audio category: volume, balance, treble, bass, pipAudio,
lineOut, surround, audioInput.
-}
audioCommands :: Category
audioCommands = Category
    "Audio"
    [ volume, balance, treble, bass, pipAudio, lineOut, surround, audioInput ]
