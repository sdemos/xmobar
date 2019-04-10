module Plugins.Bars.Common (loop, backgroundBar, backgroundBarColor) where

import Commands (tenthSeconds)

loop :: Int -> IO String -> (String -> IO ()) -> IO ()
loop rate fn cb = fn >>= cb >> (tenthSeconds rate) >> loop rate fn cb

backgroundBar :: String -> Int -> Int -> String -> String
backgroundBar = backgroundBarColor "black,grey"

backgroundBarColor :: String -> String -> Int -> Int -> String -> String
backgroundBarColor color n width percent value = bar
  where maxname = width - 3 - length value
        name = shorten maxname n
        used = length name + length value
        rawbar = concat [" ", name, replicate (width-used-2) ' ', value, " "]
        insertAt z y xs = let (as,bs) = splitAt z xs in as ++ y ++ bs
        pos = percent `div` (100 `div` width)
        bar = "<fc=" ++ color ++ ">" ++ insertAt (pos) "</fc>" rawbar

-- unapologetically stolen from xmonad-contrib
shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = take (n - length end) xs ++ end
 where
    end = "..."
