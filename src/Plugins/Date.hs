{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Date
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin for Xmobar
--
-- Usage example: in template put
--
-- > Run Date "%a %b %_d %Y <fc=#ee9a00> %H:%M:%S</fc>" "Mydate" 10
--
-----------------------------------------------------------------------------

module Plugins.Date (Date(..)) where

import Plugins

#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import "time" Data.Time

data Date = Date String String Int
    deriving (Read, Show)

instance Exec Date where
    alias (Date _ a _) = a
    run   (Date f _ _) = date f
    rate  (Date _ _ r) = r

date :: String -> IO String
date format = fmap (formatTime defaultTimeLocale format) getZonedTime
