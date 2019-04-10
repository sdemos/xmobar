module Plugins.Bars.Cpu (startCpu) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Plugins.Bars.Common

type CpuDataRef = IORef [Int]

-- cpuParser gets the list of numbers in the first line of a bytestring in the
-- /proc/stat syntax
cpuParser :: B.ByteString -> [Int]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines

-- cpuData retrieves the list of numbers in the first line of /proc/stat
cpuData :: IO [Int]
cpuData = cpuParser `fmap` B.readFile "/proc/stat"

-- parseCpu does some stupid math to turn the cpu data into relevant percentages
-- of current cpu usage
percentCpu :: CpuDataRef -> IO Float
percentCpu cref = do
  a <- readIORef cref
  b <- cpuData
  writeIORef cref b
  let dif = zipWith (-) b a
      tot = fromIntegral $ sum dif
      percent = sum $ take 3 $ map ((/ tot) . fromIntegral) dif
  pure (percent * 100)

runCpu :: CpuDataRef -> IO String
runCpu cref = cpuBar <$> round <$> percentCpu cref
  where cpuBar percent = backgroundBar "cpu" 25 percent (show percent ++ "%")

loopCpu :: CpuDataRef -> (String -> IO ()) -> IO ()
loopCpu = loop 10 . runCpu

startCpu :: (String -> IO ()) -> IO ()
startCpu cb = do
  cref <- newIORef []
  _ <- percentCpu cref
  loopCpu cref cb
