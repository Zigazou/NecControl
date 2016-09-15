{- |
Module      : All
Description : All supported commands
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

All supported commands grouped by category.
-}
module Network.NecControl.Parameters.All (allParameters) where

import Network.NecControl.Parameters.Types (Category)
 
import Network.NecControl.Parameters.Adjust
import Network.NecControl.Parameters.AdvancedOption
import Network.NecControl.Parameters.Audio
import Network.NecControl.Parameters.CarbonFootprint
import Network.NecControl.Parameters.DisplayProtection
import Network.NecControl.Parameters.MultiDisplay
import Network.NecControl.Parameters.Osd
import Network.NecControl.Parameters.Picture
import Network.NecControl.Parameters.Pip
import Network.NecControl.Parameters.Schedule
import Network.NecControl.Parameters.Settings
import Network.NecControl.Parameters.TemperatureSensor

{-|
List of all commands grouped by category: adjust, advancedOption, audio,
carbonFootprint, displayProtection, multiDisplay, osd, picture, pip, schedule,
settings, temperatureSensor
-}
allParameters :: [Category]
allParameters =
    [ adjustCommands
    , advancedOptionCommands
    , audioCommands
    , carbonFootprintCommands
    , displayProtectionCommands
    , multiDisplayCommands
    , osdCommands
    , pictureCommands
    , pipCommands
    , scheduleCommands
    , settingsCommands
    , temperatureSensorCommands
    ]
