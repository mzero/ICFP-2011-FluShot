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

import LTG.Game
import LTG.Play
import Main.Players
import Main.Utils
import System.IO

altMain :: IO ()
altMain = interactiveMain$ twoPlayerTurn (interactivePlayer "0") (interactivePlayer "1")

onlyMain :: IO ()
onlyMain = interactiveMain $ onePlayerTurn (interactivePlayer "0")


interactiveMain :: Turn -> IO ()
interactiveMain t = do
    putStrLn "FluShot LTG"
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    takeTurns $ interactiveTurn t


interactiveTurn :: Turn -> Turn
interactiveTurn t = \n s -> do
    putStrLn $ "###### turn " ++ show n
    t n s


interactivePlayer :: String -> Player
interactivePlayer p s = do
    putStrLn $ "*** player " ++ p ++ "'s turn, with slots:"
    mapM_ putStrLn $ showProState s
    putStrLn "(1) apply card to slot, or (2) apply slot to card?"
    ma <- readApply `fmap` getLine
    case ma of
        Nothing -> reportError "please specify 1 or 2"
        Just LeftApply -> cardThenSlot
        Just RightApply -> slotThenCard
  where
    reportError msg = putStrLn ("Exception: Failure(" ++ show msg ++ ")") >> return nullMove
    reportApply a b = putStrLn $ "player " ++ p ++ " applied " ++ a ++ " to " ++ b
    cardThenSlot = do
        putStrLn "card name?"
        mc <- readCard `fmap` getLine
        case mc of
            Nothing -> reportError "unknown card"
            Just c@(cn, _) -> do
                putStrLn "slot no?"
                ms <- readSlot `fmap` getLine
                case ms of
                    Nothing -> reportError "not a slot"
                    Just i -> do
                        reportApply ("card " ++ cn) ("slot " ++ show i)
                        return $ LeftMove c i

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
                    Just c@(cn, _) -> do
                        reportApply ("slot " ++ show i) ("card " ++ cn)
                        return $ RightMove i c





showProState :: State -> [String]
showProState = trailer . map showSlot . filter (not . normal) . zip [(0::Int)..] . proSlots
  where
    normal (_, s) = slVitality s == 10000 && valueName (slField s) == "I"
    showSlot (i, s) = show i ++ "={" ++ show (slVitality s) ++ "," ++ valueName (slField s) ++ "}"
    trailer = (++ ["(slots {10000,I} are omitted)"])

