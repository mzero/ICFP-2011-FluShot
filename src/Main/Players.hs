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
    Turn,
    twoPlayerTurn, onePlayerTurn,
    takeTurns,

    playMain0,
    playMain1,
) where

import LTG.Cards
import LTG.Game
import LTG.Play
import Main.Utils

type Player = State -> IO Move
type Turn = Int -> State -> IO State


playMain0, playMain1 :: IO ()
playMain0 = takeTurns $ twoPlayerTurn (stdoutEcho nullPlayer) stdinPlayer
playMain1 = takeTurns $ twoPlayerTurn stdinPlayer (stdoutEcho nullPlayer)




twoPlayerTurn :: Player -> Player -> Turn
twoPlayerTurn p0 p1 = \n s -> do
    s0 <- switchSides `fmap` onePlayerTurn p0 n s
    s1 <- switchSides `fmap` onePlayerTurn p1 n s0
    return s1

onePlayerTurn :: Player -> Turn
onePlayerTurn p = \_n s -> do
    m <- p s
    return $ execute (play m) s


takeTurns :: Turn -> IO ()
takeTurns t = go 1 initState
  where
    go n s = t n s >>= go (n + 1)



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




