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
module Network.NecControl.Commands.All (allCommands) where

import Network.NecControl.Commands.Types (Category)
 
import Network.NecControl.Commands.Adjust
import Network.NecControl.Commands.AdvancedOption
import Network.NecControl.Commands.Audio
import Network.NecControl.Commands.CarbonFootprint
import Network.NecControl.Commands.DisplayProtection
import Network.NecControl.Commands.MultiDisplay
import Network.NecControl.Commands.Osd
import Network.NecControl.Commands.Picture
import Network.NecControl.Commands.Pip
import Network.NecControl.Commands.Schedule
import Network.NecControl.Commands.Settings
import Network.NecControl.Commands.TemperatureSensor

{-|
List of all commands grouped by category: adjust, advancedOption, audio,
carbonFootprint, displayProtection, multiDisplay, osd, picture, pip, schedule,
settings, temperatureSensor
-}
allCommands :: [Category]
allCommands =
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
