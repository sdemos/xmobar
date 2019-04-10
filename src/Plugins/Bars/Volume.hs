module Plugins.Bars.Volume (startVolume) where

import Commands (tenthSeconds)
import Data.Word (Word32)
import Plugins.Bars.Common
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple
import Sound.Pulse.Sinkinfo
import Sound.Pulse.Subscribe
import Sound.Pulse.Volume

updateBar :: (String -> IO ()) -> Sinkinfo -> IO ()
updateBar cb info = cb $ volumeBar (siMute info) (getVolumePercent info)
  where volumeBar m percent = backgroundBar "volume" 25 percent (label m percent)
        label m percent = if m then "(mute)" else show percent ++ "%"

getVolumePercent :: Sinkinfo -> Int
getVolumePercent info = round $ (vol * 100 / norm)
  where vol  = let CVolume vols = siVolume info
                in sum (map toRational vols) / 2.0
        norm = 0x10000 -- don't ask me, it's hard-coded into pulseaudio

update :: Context -> (Sinkinfo -> IO ()) -> IO ()
update c cb = getContextSinkByName c "@DEFAULT_SINK@" cb >> pure ()

subCB :: Context -> (Sinkinfo -> IO ()) -> (SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ()
subCB c cb (_, ty) _ = if ty /= SubscriptionEventChange then pure () else update c cb

subscribe :: Context -> (Sinkinfo -> IO ()) -> IO ()
subscribe c cb = subscribeEvents c [SubscriptionMaskSink, SubscriptionMaskServer] (subCB c cb) >> pure ()

onceReady :: Context -> (Sinkinfo -> IO ()) -> IO ()
onceReady c cb = update c cb >> subscribe c cb >> pure ()

stateCB :: Context -> (Sinkinfo -> IO ()) -> IO ()
stateCB c cb = getContextState c >>= stateReady
  where stateReady ContextReady = onceReady c cb
        stateReady _            = pure ()

-- volume works in a _completely_ different way from the rest of the bars. the
-- pulseaudio api is callback spaghetti, but we can use that to our advantage
-- since that's how xmobar works too. essentially we only update the bar when
-- the volume changes, instead of polling every tenth of a second. yay!
startVolume :: (String -> IO ()) -> IO ()
startVolume cb = do
  -- the pulseaudio api was not very straightforward to figure out. first we get
  -- an implementation of the pulseaudio mainloop.
  pl <- getMainloopImpl
  -- next, we create a new context object. this context object will get threaded
  -- through all our function calls and allow us to get the state of the world
  -- when we need to update
  c <- getContext pl "xmobar"
  -- we set the state callback so that when the context state changes, we know
  -- about it. this function is the thing that sets up the sink state change
  -- subscription once the context is connected to the server.
  setStateCallback c (stateCB c (updateBar cb))
  -- in order for the context to be useful, it needs to be connected to the
  -- pulseaudio server. using Nothing as the server argument means it uses the
  -- default server. I don't know what the flags do.
  _ <- connectContext c Nothing [ContextNoautospawn, ContextNofail]
  -- then we start the pulse mainloop. as far as I can tell, this function never
  -- returns. the start function is already called on a new thread though (this
  -- function is supposed to block) so it's actually what we want.
  _ <- doLoop pl
  -- strangely, doLoop returns an int (??). I can only assume it will return if
  -- there is an error. in the minimal testing I did, that didn't seem to
  -- happen. but in case it does, we sleep for a second and then try again.
  tenthSeconds 10
  startVolume cb
