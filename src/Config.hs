{-# LANGUAGE TypeOperators, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The configuration module of Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Config
    ( -- * Configuration
      -- $config
      Config (..)
    , XPosition (..), Align (..), Border(..)
    , defaultConfig
    , runnableTypes
    ) where


import Commands
import {-# SOURCE #-} Runnable
import Plugins.Monitors
import Plugins.Date
import Plugins.PipeReader
import Plugins.BufferedPipeReader
import Plugins.MarqueePipeReader
import Plugins.CommandReader
import Plugins.StdinReader
import Plugins.XMonadLog
import Plugins.EWMH
import Plugins.Kbd
import Plugins.Locks
import Plugins.Bars

#ifdef INOTIFY
import Plugins.Mail
import Plugins.MBox
#endif

#ifdef DATEZONE
import Plugins.DateZone
#endif

-- $config
-- Configuration data type and default configuration

-- | The configuration data type
data Config =
    Config { font :: String         -- ^ Font
           , additionalFonts :: [String] -- ^ List of alternative fonts
           , wmClass :: String      -- ^ X11 WM_CLASS property value
           , wmName :: String       -- ^ X11 WM_NAME property value
           , bgColor :: String      -- ^ Backgroud color
           , fgColor :: String      -- ^ Default font color
           , position :: XPosition  -- ^ Top Bottom or Static
           , textOffset :: Int      -- ^ Offset from top of window for text
           , textOffsets :: [Int]   -- ^ List of offsets for additionalFonts
           , iconOffset :: Int      -- ^ Offset from top of window for icons
           , border :: Border       -- ^ NoBorder TopB BottomB or FullB
           , borderColor :: String  -- ^ Border color
           , borderWidth :: Int     -- ^ Border width
           , alpha :: Int           -- ^ Transparency from 0 (transparent) to 255 (opaque)
           , hideOnStart :: Bool    -- ^ Hide (Unmap) the window on
                                    --   initialization
           , allDesktops :: Bool    -- ^ Tell the WM to map to all desktops
           , overrideRedirect :: Bool -- ^ Needed for dock behaviour in some
                                      --   non-tiling WMs
           , pickBroadest :: Bool   -- ^ Use the broadest display
                                    --   instead of the first one by
                                    --   default
           , lowerOnStart :: Bool   -- ^ lower to the bottom of the
                                    --   window stack on initialization
           , persistent :: Bool     -- ^ Whether automatic hiding should
                                    --   be enabled or disabled
           , iconRoot :: FilePath   -- ^ Root folder for icons
           , commands :: [Runnable] -- ^ For setting the command,
                                    --   the command arguments
                                    --   and refresh rate for the programs
                                    --   to run (optional)
           , sepChar :: String      -- ^ The character to be used for indicating
                                    --   commands in the output template
                                    --   (default '%')
           , alignSep :: String     -- ^ Separators for left, center and
                                    --   right text alignment
           , template :: String     -- ^ The output template
           } deriving (Read)

data XPosition = Top
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
                 deriving ( Read, Eq )

data Align = L | R | C deriving ( Read, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Read, Eq )

-- | The default configuration values
defaultConfig :: Config
defaultConfig =
    Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , additionalFonts = []
           , wmClass = "xmobar"
           , wmName = "xmobar"
           , bgColor = "#000000"
           , fgColor = "#BFBFBF"
           , alpha   = 255
           , position = Top
           , border = NoBorder
           , borderColor = "#BFBFBF"
           , borderWidth = 1
           , textOffset = -1
           , iconOffset = -1
           , textOffsets = []
           , hideOnStart = False
           , lowerOnStart = True
           , persistent = False
           , allDesktops = True
           , overrideRedirect = True
           , pickBroadest = False
           , iconRoot = "."
           , commands = [ Run $ Date "%a %b %_d %Y * %H:%M:%S" "theDate" 10
                        , Run StdinReader]
           , sepChar = "%"
           , alignSep = "}{"
           , template = "%StdinReader% }{ " ++
                        "<fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>"
           }


-- | An alias for tuple types that is more convenient for long lists.
type a :*: b = (a, b)
infixr :*:

-- | This is the list of types that can be hidden inside
-- 'Runnable.Runnable', the existential type that stores all commands
-- to be executed by Xmobar. It is used by 'Runnable.readRunnable' in
-- the 'Runnable.Runnable' Read instance. To install a plugin just add
-- the plugin's type to the list of types (separated by ':*:') appearing in
-- this function's type signature.
runnableTypes :: Command :*: Monitors :*: Date :*: PipeReader :*:
                 BufferedPipeReader :*: CommandReader :*: StdinReader :*:
                 XMonadLog :*: EWMH :*: Kbd :*: Locks :*: Bars :*:
#ifdef INOTIFY
                 Mail :*: MBox :*:
#endif
#ifdef DATEZONE
                 DateZone :*:
#endif
                 MarqueePipeReader :*: ()
runnableTypes = undefined
