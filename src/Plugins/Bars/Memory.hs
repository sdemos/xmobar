module Plugins.Bars.Memory (startMemory) where

import qualified Data.Map as M
import Plugins.Bars.Common

parseMem :: String -> Float
parseMem contents = (used / total) * 100
  where info = M.fromList
             $ map (\line -> (head line, (read $ line !! 1 :: Float) / 1024))
             $ map words
             $ take 8
             $ lines contents
        [total, free, buffer, cache] = map (info M.!) ["MemTotal:", "MemFree:", "Buffers:", "Cached:"]
        available = M.findWithDefault (free + buffer + cache) "MemAvailable:" info
        used = total - available

percentMem :: IO Float
percentMem = parseMem <$> readFile "/proc/meminfo"

runMemory :: IO String
runMemory = memBar <$> round <$> percentMem
  where memBar percent = backgroundBar "memory" 25 percent (show percent ++ "%")

startMemory :: (String -> IO ()) -> IO ()
startMemory = loop 10 runMemory
