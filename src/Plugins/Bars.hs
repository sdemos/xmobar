module Plugins.Bars where

import Plugins
import Plugins.Bars.Cpu (startCpu)
import Plugins.Bars.Memory (startMemory)
import Plugins.Bars.Brightness (startBrightness)
import Plugins.Bars.Volume (startVolume)
import Plugins.Bars.Battery (startBattery)
import Plugins.Bars.Network (startNetwork)

data Bars = Cpu
          | Memory
          | Brightness
          | Volume
          | Battery
          | Network
  deriving (Show, Read)

instance Exec Bars where
  alias Cpu           = "cpu"
  alias Memory        = "memory"
  alias Brightness    = "brightness"
  alias Volume        = "volume"
  alias Battery       = "battery"
  alias Network       = "network"
  start Cpu           = startCpu
  start Memory        = startMemory
  start Brightness    = startBrightness
  start Volume        = startVolume
  start Battery       = startBattery
  start Network       = startNetwork
