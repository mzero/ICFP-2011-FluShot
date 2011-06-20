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
    liveMatchMain,
) where

import Control.Concurrent
import Control.Exception
import LTG.Game
import Main.Match
import Main.Players
import System.Exit
import UI.HSCurses.Curses


liveMatchMain :: String -> String -> IO ()
liveMatchMain cmd0 cmd1 = do
    mds <- newMVar initDisplayState
    (p0, p1, cleanup) <- makeMatchPlayers cmd0 cmd1
    let lp0 = livePlayer mds LeftSide p0
    let lp1 = livePlayer mds RightSide p1
    liveMain mds (liveTurn $ twoPlayerTurn lp0 lp1) cleanup


data Side = LeftSide | RightSide
data DisplayState = DS { dsPage :: Int }
initDisplayState :: DisplayState
initDisplayState = DS { dsPage = 0 }

liveMain :: MVar DisplayState -> Turn -> IO() -> IO ()
liveMain _mds t cleanup = do
    tid <- cleanStart
    finally (takeTurns t) (cleanup >> cleanExit tid)
  where
    cleanStart = do
        initCurses
        cBreak True
        echo False
        nl False
        intrFlush False
        keypad stdScr True
        -- forkIO $ processInput mds
        return True
    cleanExit _tid = do
        -- killThread tid
        (h, _w) <- scrSize
        move (h-1) 0
        refresh
        endWin
        exitSuccess

liveTurn :: Turn -> Turn
liveTurn t = \n s -> do
    displayLineNumber n
    t n s

livePlayer :: (MVar DisplayState) -> Side -> Player -> Player
livePlayer mds side p s = do
    ds <- readMVar mds
    displayProState ds side s
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

displayProState :: DisplayState -> Side -> State -> IO ()
displayProState ds side st = do
    (height, fullWidth) <- scrSize
    let (offset, width) = locate fullWidth
    let page = dsPage ds * 64
    mapM_ (showSlot offset width) $ zip3 [2..height-2] [page..] $ drop page $ proSlots st
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
{-
processInput :: (MVar DisplayState) -> IO ()
processInput mds = do
    key <- getCh
    putStrLn $ "### " ++ show key ++ " ###"
    case key of
        KeyChar c | '1' <= c && c <= '4' -> page (fromEnum c - fromEnum '1')
        KeyF i | 1 <= i && i <= 4 -> page (i - 1)
        _ -> return ()
    processInput mds
  where
    page n = modifyMVar_ mds (\ds -> return $ ds { dsPage = n })
-}
