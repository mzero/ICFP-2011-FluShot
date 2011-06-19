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
    playMain0,
    playMain1,
) where

import LTG.Cards
import LTG.Game
import LTG.Play
import LTG.Player
import Main.Utils

playMain0, playMain1 :: IO ()
playMain0 = playMain (stdoutEcho nullPlayer) stdinPlayer
playMain1 = playMain  stdinPlayer           (stdoutEcho nullPlayer)

playMain :: Player -> Player -> IO ()
playMain p0 p1 = turn initState $ cycle [p0, p1]
  where
    turn s (p:ps) = do
        (Move app i (_cn, cv)) <- p s
        let s' = execute (play app i cv) s
        turn (switchSides s') ps
    turn _ [] = error "exhausted cycle?!"


stdoutEcho :: Player -> Player
stdoutEcho p = \s -> do
    m@(Move app i (cn, _cv)) <- p s
    case app of
        LeftApply -> mapM_ putStrLn ["1", cn, show i]
        RightApply -> mapM_ putStrLn ["2", show i, cn]
    return m


nullPlayer :: Player
nullPlayer _ = return $ Move LeftApply 0 cardIdentity


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
        validate LeftApply ms mc
    slotThenCard = do
        ms <- readSlot `fmap` getLine
        mc <- readCard `fmap` getLine
        validate RightApply ms mc
    validate app ms mc = case (ms, mc) of
        (Nothing, _) -> error "read bad slot"
        (_, Nothing) -> error "read bad card"
        (Just i, Just c) -> return $ Move app i c




