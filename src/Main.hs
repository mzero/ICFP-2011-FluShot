-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Main.Generate
import Main.Interactive
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["alt"] -> altMain
    ["only"] -> onlyMain
    ["gen", n] -> genMain n
    _ -> putStrLn "huh?"
