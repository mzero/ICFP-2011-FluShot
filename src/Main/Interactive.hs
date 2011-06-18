-----------------------------------------------------------------------------
--
-- Module      :  Main.Interactive
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
  altMain,
  onlyMain,
) where

import Control.Monad (join)
import Data.Maybe (listToMaybe)
import LTG.Game
import LTG.Cards
import LTG.Play
import Main.Utils
import System.IO

altMain :: IO ()
altMain = interactiveStartup >> playAlt 1 initState

onlyMain :: IO ()
onlyMain = interactiveStartup >> playOneSided 1 initState

interactiveStartup :: IO ()
interactiveStartup = do
    putStrLn "FluShot LTG"
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering


playAlt :: Int -> State -> IO ()
playAlt turn s = do
    putStrLn $ "###### turn " ++ show turn
    s0 <- switchSides `fmap` playTurn "0" s
    s1 <- switchSides `fmap` playTurn "1" s0
    playAlt (turn + 1) s1


playOneSided :: Int -> State -> IO ()
playOneSided turn s = do
    putStrLn $ "###### turn " ++ show turn
    playTurn "0" s >>= playOneSided (turn + 1)


playTurn :: String -> State -> IO State
playTurn p s = do
    putStrLn $ "*** player " ++ p ++ "'s turn, with slots:"
    mapM_ putStrLn $ showProState s
    putStrLn "(1) apply card to slot, or (2) apply slot to card?"
    ma <- readApply `fmap` getLine
    case ma of
        Nothing -> reportError "please specify 1 or 2"
        Just LeftApply -> cardThenSlot
        Just RightApply -> slotThenCard
  where
    reportError msg = putStrLn ("Exception: Failure(" ++ show msg ++ ")") >> return s
    reportApply a b = putStrLn $ "player " ++ p ++ " applied " ++ a ++ " to " ++ b
    cardThenSlot = do
        putStrLn "card name?"
        mc <- readCard `fmap` getLine
        case mc of
            Nothing -> reportError "unknown card"
            Just (cn, c) -> do
                putStrLn "slot no?"
                ms <- readSlot `fmap` getLine
                case ms of
                    Nothing -> reportError "not a slot"
                    Just i -> do
                        reportApply ("card " ++ cn) ("slot " ++ show i)
                        return $ execute (play LeftApply i c) s

    slotThenCard = do
        putStrLn "slot no?"
        ms <- readSlot `fmap` getLine
        case ms of
            Nothing -> reportError "unknown card"
            Just i -> do
                putStrLn "card name?"
                mc <- readCard `fmap` getLine
                case mc of
                    Nothing -> reportError "not a slot"
                    Just (cn, c) -> do
                        reportApply ("slot " ++ show i) ("card " ++ cn)
                        return $ execute (play RightApply i c) s


readApply :: String -> Maybe Application
readApply = join . (decodeApply `fmap`) . readAs
  where
    decodeApply :: Int -> Maybe Application
    decodeApply 1 = Just LeftApply
    decodeApply 2 = Just RightApply
    decodeApply _ = Nothing

readCard :: String -> Maybe Card
readCard s =  listToMaybe $ filter ((==s).fst) cards

readSlot :: String -> Maybe Int
readSlot = join . (inRange `fmap`) . readAs
  where
    inRange i = if 0 <= i && i <= 255 then Just i else Nothing




showProState :: State -> [String]
showProState = trailer . map showSlot . filter (not . normal) . zip [(0::Int)..] . proSlots
  where
    normal (_, s) = slVitality s == 10000 && valueName (slField s) == "I"
    showSlot (i, s) = show i ++ "={" ++ show (slVitality s) ++ "," ++ valueName (slField s) ++ "}"
    trailer = (++ ["(slots {10000,I} are omitted)"])

