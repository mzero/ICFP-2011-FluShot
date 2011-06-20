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

    playMain0, playMain1,
    playRandomMain0, playRandomMain1,

    nullPlayer, randomPlayer,
    handleEcho, stdoutEcho,
    handlePlayer, stdinPlayer,
) where

import LTG.Cards
import LTG.Game
import LTG.Play
import Main.Utils
import System.IO
import System.Random

type Player = State -> IO Move
type Turn = Int -> State -> IO State


playMain0, playMain1 :: IO ()
playMain0 = competitionMain $ twoPlayerTurn (stdoutEcho nullPlayer) stdinPlayer
playMain1 = competitionMain $ twoPlayerTurn stdinPlayer (stdoutEcho nullPlayer)

playRandomMain0, playRandomMain1 :: IO ()
playRandomMain0 = competitionMain $ twoPlayerTurn (stdoutEcho randomPlayer) stdinPlayer
playRandomMain1 = competitionMain $ twoPlayerTurn stdinPlayer (stdoutEcho randomPlayer)


competitionMain :: Turn -> IO ()
competitionMain t = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    takeTurns t


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
stdoutEcho = handleEcho stdout

handleEcho :: Handle -> Player -> Player
handleEcho h p = \s -> do
    m <- p s
    let l = case m of
                LeftMove (cn, _) i -> ["1", cn, show i]
                RightMove i (cn, _) -> ["2", show i, cn]
    mapM_ (hPutStrLn h) l
    return m


stdinPlayer :: Player
stdinPlayer = handlePlayer stdin

handlePlayer :: Handle -> Player
handlePlayer h _ = do
    ma <- readApply `fmap` inLine
    case ma of
        Nothing -> error "read bad application"
        Just LeftApply -> cardThenSlot
        Just RightApply -> slotThenCard
  where
    inLine = hGetLine h
    cardThenSlot = do
        mc <- readCard `fmap` inLine
        ms <- readSlot `fmap` inLine
        validate (flip LeftMove) ms mc
    slotThenCard = do
        ms <- readSlot `fmap` inLine
        mc <- readCard `fmap` inLine
        validate RightMove ms mc
    validate mConst ms mc = case (ms, mc) of
        (Nothing, _) -> error "read bad slot"
        (_, Nothing) -> error "read bad card"
        (Just i, Just c) -> return $ mConst i c



nullPlayer :: Player
nullPlayer _ = return $ LeftMove cardIdentity 0

randomPlayer :: Player
randomPlayer _ = do
    d <- randomIO
    s <- randomRIO (0, 255) :: IO Int
    c <- (cards !!) `fmap` randomRIO (0, numCards - 1)
    case d of
        True  -> return $ LeftMove c s
        False -> return $ RightMove s c

numCards :: Int
numCards = length cards


