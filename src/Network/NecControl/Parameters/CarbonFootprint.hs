{- |
Module      : CarbonFootprint
Description : Commands of the carbon footprint category
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Commands of the carbon footprint category.
-}
module Network.NecControl.Parameters.CarbonFootprint
(carbonFootprintCommands)
where

import Network.NecControl.Parameters.Types

readoutCarbonFootprintG :: Action
readoutCarbonFootprintG = Action
    { actName = "Readout carbon footprint (g)"
    , actCommand = "carbon-footprint-g"
    , actOpCode = OpCode 0x1010
    , actConstraints = ReadOnly
    }

readoutCarbonFootprintKg :: Action
readoutCarbonFootprintKg = Action
    { actName = "Readout carbon footprint (kg)"
    , actCommand = "carbon-footprint-kg"
    , actOpCode = OpCode 0x1011
    , actConstraints = ReadOnly
    }

readoutCarbonUsageG :: Action
readoutCarbonUsageG = Action
    { actName = "Readout carbon usage (g)"
    , actCommand = "carbon-usage-g"
    , actOpCode = OpCode 0x1026
    , actConstraints = ReadOnly
    }

readoutCarbonUsageKg :: Action
readoutCarbonUsageKg = Action
    { actName = "Readout carbon usage (kg)"
    , actCommand = "carbon-usage-kg"
    , actOpCode = OpCode 0x1027
    , actConstraints = ReadOnly
    }

{-|
Commands of the carbon footprint category: readoutCarbonFootprintG,
readoutCarbonFootprintKg, readoutCarbonUsageG, readoutCarbonUsageKg.
-}
carbonFootprintCommands :: Category
carbonFootprintCommands = Category
    "Carbon footprint"
    [ readoutCarbonFootprintG, readoutCarbonFootprintKg
    , readoutCarbonUsageG, readoutCarbonUsageKg
    ]
