-----------------------------------------------------------------------------
--
-- Module      :  LTG.Play
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

module LTG.Play (
    Application(..),
    play,
    turn,
) where

import Control.Monad
import LTG.Game


data Application = LeftApply | RightApply


leftAppMove :: Int -> Value -> Exec ()
leftAppMove i c = do
    s <- getProSlot i
    v <- (precondition (alive s) >> apply c (slField s)) `mplus` return identity
    s' <- getProSlot i -- must refetch lest it's been modified
    putProSlot i $ s' { slField = v }

rightAppMove :: Int -> Value -> Exec ()
rightAppMove i c = do
    s <- getProSlot i
    v <- (precondition (alive s) >> apply (slField s) c) `mplus` return identity
    s' <- getProSlot i -- must refetch lest it's been modified
    putProSlot i $ s' { slField = v }

appMove :: Application -> Int -> Value -> Exec ()
appMove LeftApply = leftAppMove
appMove RightApply = rightAppMove

zombieMove :: Int -> Exec ()
zombieMove i = do
    s <- getProSlot i
    precondition $ zombie s
    v <- apply (slField s) identity `mplus` return identity
    putProSlot i $ Slot { slField = identity, slVitality = 0 }


activateZombies :: Exec ()
activateZombies = do
    (lo, hi) <- slotRange
    zombify $ sequence_ $ map ((`mplus` return ()).zombieMove) [lo..hi]


play :: Application -> Int -> Value -> Exec ()
play app i c = do
    activateZombies
    appMove app i c

turn :: Application -> Int -> Value -> Exec ()
turn app i c = play app i c >> switchSides

