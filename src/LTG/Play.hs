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
    Move(..),
    play,
    nullMove,
) where

import Control.Monad
import LTG.Cards
import LTG.Game


data Application = LeftApply | RightApply

data Move = LeftMove Card Int | RightMove Int Card

play :: Move -> Exec ()
play m = do
    activateZombies
    case m of
        LeftMove (_, c) i -> leftAppMove c i
        RightMove i (_, c) -> rightAppMove i c

nullMove :: Move                    -- a move that does nothing
nullMove = LeftMove cardZero 0      -- always fails with no side-effects



leftAppMove :: Value -> Int -> Exec ()
leftAppMove c i = do
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


zombieMove :: Int -> Exec ()
zombieMove i = do
    s <- getProSlot i
    precondition $ zombie s
    _v <- apply (slField s) identity `mplus` return identity
    putProSlot i $ Slot { slField = identity, slVitality = 0 }


activateZombies :: Exec ()
activateZombies = do
    (lo, hi) <- slotRange
    zombify $ sequence_ $ map ((`mplus` return ()).zombieMove) [lo..hi]





