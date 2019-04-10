module Plugins.Bars.Network (startNetwork) where

import GHC.IO.Handle (hGetContents)
import Plugins.Bars.Common
import System.Process (createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess)

network :: IO String
network = do
  (_, Just hout, _, handle) <-
    createProcess (proc "nmcli" ["-t", "c", "show", "--active"])
    { std_out = CreatePipe }
  waitForProcess handle
  hGetContents hout

-- returns the name of the network we are connected to, parsed out of the output
-- of nmcli -t c show --active
parseNetwork :: String -> String
parseNetwork = f "(no wifi)" . fst . break (== ':')
    where f def "" = def
          f _   x  = x

runNetwork :: IO String
runNetwork = fmap parseNetwork network

startNetwork :: (String -> IO ()) -> IO ()
startNetwork = loop 10 runNetwork
