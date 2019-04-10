module Plugins.Bars.Brightness (startBrightness) where

import Plugins.Bars.Common

curBrightness :: IO Float
curBrightness = read <$> readFile "/sys/class/backlight/intel_backlight/brightness"

maxBrightness :: IO Float
maxBrightness = read <$> readFile "/sys/class/backlight/intel_backlight/max_brightness"

percentBrightness :: IO Float
percentBrightness = (/) <$> curBrightness <*> maxBrightness

runBrightness :: IO String
runBrightness = brightBar <$> round <$> (*100) <$> percentBrightness
  where brightBar percent = backgroundBar "brightness" 25 percent (show percent ++ "%")

startBrightness :: (String -> IO ()) -> IO ()
startBrightness = loop 1 runBrightness
