-----------------------------------------------------------------------------
--
-- Module      :  Main.Only
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

module Main.Interactive (
  onlyMain
) where

import Control.Monad (join)
import Data.Maybe (listToMaybe)
import LTG.Game
import LTG.Cards
import LTG.Play
import System.IO

onlyMain :: IO ()
onlyMain = do
    putStrLn "FluShot LTG"
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    playOneSided 1 initState

playOneSided :: Int -> State -> IO ()
playOneSided turn s = do
    putStrLn $ "###### turn " ++ show turn
    putStrLn "*** player 0's turn, with slots:"
    mapM_ putStrLn $ showProState s
    putStrLn "(1) apply card to slot, or (2) apply slot to card?"
    ma <- readApply `fmap` getLine
    case ma of
        Nothing -> reportError "please specify 1 or 2" >> again
        Just LeftApply -> cardThenSlot
        Just RightApply -> slotThenCard
  where
    reportError s = putStrLn $ "Exception: Failure(" ++ show s ++ ")"
    again = playOneSided turn s
    cardThenSlot = do
        putStrLn "card name?"
        mc <- readCard `fmap` getLine
        case mc of
            Nothing -> reportError "unknown card" >> again
            Just c -> do
                putStrLn "slot no?"
                ms <- readSlot `fmap` getLine
                case ms of
                    Nothing -> reportError "not a slot" >> again
                    Just i -> let s' = execute (play LeftApply i c) s in
                              playOneSided (turn + 1) s'

    slotThenCard = do
        putStrLn "slot no?"
        ms <- readSlot `fmap` getLine
        case ms of
            Nothing -> reportError "unknown card" >> again
            Just i -> do
                putStrLn "card name?"
                mc <- readCard `fmap` getLine
                case mc of
                    Nothing -> reportError "not a slot" >> again
                    Just c -> let s' = execute (play RightApply i c) s in
                              playOneSided (turn + 1) s'


readApply :: String -> Maybe Application
readApply = join . (decodeApply `fmap`) . readAs
  where
    decodeApply 1 = Just LeftApply
    decodeApply 2 = Just RightApply
    decodeApply _ = Nothing

readCard :: String -> Maybe Value
readCard =  (`lookup` cards)

readSlot :: String -> Maybe Int
readSlot = join . (inRange `fmap`) . readAs
  where
    inRange i = if 0 <= i && i <= 255 then Just i else Nothing

readAs :: (Read a) => String -> Maybe a
readAs = (fst `fmap`) . listToMaybe . filter (null.snd) . reads



showProState :: State -> [String]
showProState = trailer . map showSlot . filter (not . normal) . zip [0..] . proSlots
  where
    normal (i, s) = slVitality s == 10000 && valueName (slField s) == "I"
    showSlot (i, s) = show i ++ "={" ++ show (slVitality s) ++ "," ++ valueName (slField s) ++ "}"
    trailer = (++ ["(slots {10000,I} are omitted)"])

