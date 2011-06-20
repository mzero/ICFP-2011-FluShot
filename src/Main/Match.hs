-----------------------------------------------------------------------------
--
-- Module      :  Main.Match
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

module Main.Match (
    matchMain,
    makeMatchPlayers,
) where

import Control.Exception
import Main.Players
import System.IO
import System.Process

matchMain :: String -> String -> IO ()
matchMain cmd0 cmd1 = do
    (p0, p1, cleanup) <- makeMatchPlayers cmd0 cmd1
    finally (takeTurns $ twoPlayerTurn p0 p1) cleanup


makeMatchPlayers :: String -> String -> IO (Player, Player, IO ())
makeMatchPlayers cmd0 cmd1 = do
    (in0, out0, _err0, pid0) <- runInteractiveCommand $ cmd0 ++ " 0"
    (in1, out1, _err1, pid1) <- runInteractiveCommand $ cmd1 ++ " 1"
    mapM_ (\h -> hSetBuffering h NoBuffering) [in0, out0, in1, out1]
    let cleanup = terminateProcess pid0 >> terminateProcess pid1

    let p0 = handleEcho in1 $ handlePlayer out0
    let p1 = handleEcho in0 $ handlePlayer out1

    return (p0, p1, cleanup)
