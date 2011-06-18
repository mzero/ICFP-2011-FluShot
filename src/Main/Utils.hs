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
    readAs
) where

import Data.Maybe (listToMaybe)


readAs :: (Read a) => String -> Maybe a
readAs = (fst `fmap`) . listToMaybe . filter (null.snd) . reads

