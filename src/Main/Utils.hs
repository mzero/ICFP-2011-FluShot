-----------------------------------------------------------------------------
--
-- Module      :  Main.Utils
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

module Main.Utils (
    readAs,
    readApply, readCard, readSlot,
) where

import Control.Monad (join)
import Data.Maybe (listToMaybe)
import LTG.Cards
import LTG.Play

readAs :: (Read a) => String -> Maybe a
readAs = (fst `fmap`) . listToMaybe . filter (null.snd) . reads

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

