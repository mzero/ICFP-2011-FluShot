-----------------------------------------------------------------------------
--
-- Module      :  LTG.Player
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

module LTG.Player (
    Move(..),
    Player
) where

import LTG.Cards
import LTG.Game
import LTG.Play


data Move = Move Application Int Card

type Player = State -> IO Move
