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
import Main.Live
import Main.Match
import Main.Players
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    -- tournament master modes
    ["alt"] -> altMain                  -- interactive input
    ["only"] -> onlyMain                -- interactive input, one player
    ["match", a, b] -> matchMain a b    -- match two players
    ["live", a, b] -> liveMatchMain a b -- match two player, live display

    -- players
    ["0"] -> playMain0
    ["1"] -> playMain1
    ["random", "0"] -> playRandomMain0
    ["random", "1"] -> playRandomMain1
    ["gen", n] -> genMain n

    _ -> putStrLn "huh?"
