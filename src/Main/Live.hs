-----------------------------------------------------------------------------
--
-- Module      :  Main.Live
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

module Main.Live (
    liveTournamentMain,
) where

import Control.Exception
import LTG.Game
import Main.Players
import System.Exit
import UI.HSCurses.Curses


liveTournamentMain :: IO ()
liveTournamentMain = liveMain $ twoPlayerTurn p0 p1
  where
    p0 = livePlayer LeftSide stdinPlayer
    p1 = livePlayer RightSide stdinPlayer



data Side = LeftSide | RightSide


liveMain :: Turn -> IO ()
liveMain t = do
    initCurses
    finally
        (takeTurns $ liveTurn t)
        cleanExit
  where
    cleanExit = do
        (h, _w) <- scrSize
        move (h-1) 0
        refresh
        update
        endWin
        exitSuccess

liveTurn :: Turn -> Turn
liveTurn t = \n s -> do
    displayLineNumber n
    t n s

livePlayer :: Side -> Player -> Player
livePlayer side p s = do
    displayProState side s
    m <- p s
    -- report move
    -- hilight state line
    return m


displayLineNumber :: Int -> IO ()
displayLineNumber n = do
    let s = "== " ++ show n ++ " =="
    (_h, w) <- scrSize
    mvWAddStr stdScr 0 ((w - length s) `div` 2) s
    updateDisplay

displayProState :: Side -> State -> IO ()
displayProState side st = do
    (height, fullWidth) <- scrSize
    let (offset, width) = locate fullWidth
    mapM_ (showSlot offset width) $ zip3 [2..height-2] [(0::Int)..] $ proSlots st
    updateDisplay
  where
    locate w = (case side of LeftSide -> 0; RightSide -> w`div`2, w`div`2)
    showSlot x w (y, i, s) = mvWAddStr stdScr y x $ formatSlot w i s
    formatSlot w i s = case side of
        LeftSide -> rightAlign 5 (show $ slVitality s)
                        ++ " [" ++ rightAlign 3 (show i) ++ "] "
                        ++ leftAlign (w - 13) (valueName $ slField s)
        RightSide -> rightAlign (w - 13) (valueName $ slField s)
                        ++ " [" ++ rightAlign 3 (show i) ++ "] "
                        ++ leftAlign 5 (show $ slVitality s)

leftAlign, rightAlign :: Int -> String -> String
leftAlign n s = take n $ s ++ repeat ' '
rightAlign n s = take n $ replicate (n - length s) ' ' ++ s

updateDisplay :: IO ()
updateDisplay = do
    move 0 0
    refresh
    update


