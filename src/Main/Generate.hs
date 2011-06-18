-----------------------------------------------------------------------------
--
-- Module      :  Main.Generate
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

module Main.Generate (
    genMain
) where

import Control.Monad
import LTG.Cards
import Main.Utils
import System.Random


genMain :: String -> IO ()
genMain s = do
    case readAs s of
        Nothing -> putStrLn "supply a count"
        Just n -> replicateM_ n randomMove

randomMove :: IO ()
randomMove = do
    d <- randomIO
    s <- randomRIO (0, 255) :: IO Int
    c <- randomRIO (0, numCards - 1)
    let l = case d of
                True  -> ["1", cardNames !! c, show s]
                False -> ["2", show s, cardNames !! c]
    mapM_ putStrLn l


cardNames :: [String]
cardNames = map fst cards

numCards :: Int
numCards = length cards
