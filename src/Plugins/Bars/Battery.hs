module Plugins.Bars.Battery (startBattery) where

import Data.Char (isDigit)
import GHC.IO.Handle (hGetContents)
import Plugins.Bars.Common
import System.Process (createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess)

acpi :: IO String
acpi = do
  (_, Just hout, _, handle) <- createProcess (proc "acpi" []){ std_out = CreatePipe }
  waitForProcess handle
  hGetContents hout

-- Battery 0: Discharging, 60%, 04:49:03 remaining
-- Battery 0: Unknown, 94%
-- Battery 0: Charging, 94%, 00:15:38 until charged
-- generally -
--   Battery <num>: <state> <percentage>%[, <02hours>:<02minutes>:<02seconds>][<poststr>]
--   num: probably 0, maybe 1 on a machine with two batteries
--   state: [Discharging|Unknown|Charging|Full] but maybe more, I can't seem to
--       find a complete list.
--   percentage: 1 or 2 digit number representing percentage of charge in battery
--   hours: 2 digit number of hours left in charge, optional
--   minutes: 2 digit number of minutes left in charge, optional
--   seconds: 2 digit number of seconds left in charge, optional
--   poststr: random additional english relating to battery charge rate
parseAcpi :: String -> (Int, String, String)
parseAcpi v = (percent, time, color)
  where info = drop 2 (words v)
        status' = info !! 0
        status = if status' == "Not" then status' ++ init (info !! 1) else init status'
        moving = status == "Charging" || status == "Discharging"
        -- this isn't great
        percentIndex = if status == "Not Charging" then 2 else 1
        percent = read $ (if moving then init else id) $ init $ info !! percentIndex
        -- time left might not exist. check if we are actually Charging or
        -- Discharging.
        timeIndex = if status == "Not Charging" then 3 else 2
        time = if moving then time' (info !! timeIndex) else ""
        time' s = if isDigit (s !! 0)
             then let (hours', rest) = break (== ':') s
               in let (minutes', _) = break (== ':') (tail rest)
               in let hours = read hours' :: Int
               in let minutes = read minutes' :: Int
               in (if hours == 0 then "" else show hours ++ "h ") ++ show minutes ++ "m"
             else ""
        color = case status of
          "Charging" -> "black,#80c080"
          "Discharging" -> "black,#c08080"
          _ -> "black,grey"

runBattery :: IO String
-- runBattery = undefined
runBattery = batteryBar <$> parseAcpi <$> acpi
  where batteryBar (p,t,c) = backgroundBarColor c "battery" 25 p t

startBattery :: (String -> IO ()) -> IO ()
startBattery = loop 10 runBattery
