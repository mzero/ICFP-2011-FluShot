-----------------------------------------------------------------------------
--
-- Module      :  Main.Players
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

module Main.Players (
    Player,
    playMain0,
    playMain1,
) where

import LTG.Cards
import LTG.Game
import LTG.Play
import Main.Utils

type Player = State -> IO Move


playMain0, playMain1 :: IO ()
playMain0 = playMain (stdoutEcho nullPlayer) stdinPlayer
playMain1 = playMain  stdinPlayer           (stdoutEcho nullPlayer)

playMain :: Player -> Player -> IO ()
playMain p0 p1 = turn initState $ cycle [p0, p1]
  where
    turn s (p:ps) = do
        m <- p s
        let s' = execute (play m) s
        turn (switchSides s') ps
    turn _ [] = error "exhausted cycle?!"


stdoutEcho :: Player -> Player
stdoutEcho p = \s -> do
    m <- p s
    let l = case m of
                LeftMove (cn, _) i -> ["1", cn, show i]
                RightMove i (cn, _) -> ["2", show i, cn]
    mapM_ putStrLn l
    return m


nullPlayer :: Player
nullPlayer _ = return $ LeftMove cardIdentity 0


stdinPlayer :: Player
stdinPlayer _ = do
    ma <- readApply `fmap` getLine
    case ma of
        Nothing -> error "read bad application"
        Just LeftApply -> cardThenSlot
        Just RightApply -> slotThenCard
  where
    cardThenSlot = do
        mc <- readCard `fmap` getLine
        ms <- readSlot `fmap` getLine
        validate (flip LeftMove) ms mc
    slotThenCard = do
        ms <- readSlot `fmap` getLine
        mc <- readCard `fmap` getLine
        validate RightMove ms mc
    validate mConst ms mc = case (ms, mc) of
        (Nothing, _) -> error "read bad slot"
        (_, Nothing) -> error "read bad card"
        (Just i, Just c) -> return $ mConst i c




