-----------------------------------------------------------------------------
--
-- Module      :  LTG.Cards
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

module LTG.Cards (
    cards
) where

import LTG.Types


cards :: [(String, Value)]
cards = [ cardIdentity
        , cardZero
        , cardSucc
        , cardDouble
        , cardGet
        , cardPut
        , cardS
        , cardK
        , cardInc
        , cardDec
        , cardAttack
        , cardHelp
        , cardCopy
        , cardRevive
        , cardZombie
        ]
  where c card = (valueName card, card)

card n v = (n, v)
card1 n f = card n $ Func1 n f
card2 n f = card n $ Func2 n f
card3 n f = card n $ Func3 n f

cardIdentity = card "I" identity

cardZero = card "zero" (Num 0)

cardSucc = card1 "succ" (numFunc (+1))
cardDouble = card1 "dbl" (numFunc (*2))


numFunc :: (Int -> Int) -> Value -> Exec Value
numFunc f (Num i) = return (Num i')
  where
    i' = min 65535 (max 0 (f i))
numFunc _ _ = execError


cardGet = card1 "get" get
  where
    get (Num i) = do
        s <- getProSlot i
        if alive s
          then return (slField s)
          else execError
    get _ = execError

cardPut = card1 "put" put
  where
    put _ = return identity

cardS = card3 "S" s
  where
    s f g x = do
      h <- apply f x
      y <- apply g x
      apply h y

cardK = card2 "K"  k
  where
    k x y = return x


cardInc = card1 "inc" inc
  where
    inc (Num i) = do
        s <- getProSlot i
        let s' = adjustVitality (+1) s
        putProSlot i s'
        return identity
    inc _ = execError

cardDec = card1 "dec" dec
  where
    dec (Num i) = do
        s <- getOpSlotRev i
        let s' = adjustVitality (-1+) s
        putOpSlotRev i s'
        return identity
    dec _ = execError

adjustVitality :: (Int -> Int) -> Slot -> Slot
adjustVitality f s = s { slVitality = v' }
  where
    v = slVitality s
    v' = if v > 0 then min 65535 (max 0 (f v)) else v

cardAttack = card3 "attack" attack
  where
    attack (Num i) (Num j) (Num n) = do
      s <- getProSlot i
      if slVitality s < n
        then execError
        else do
          let s' = adjustVitality (subtract n) s
          putProSlot i s'
          t <- getOpSlotRev j
          let t' = adjustVitality (subtract (n*9`div`10)) t
          putOpSlotRev j t'
          return identity
    attack _ _ _ = execError

cardHelp = card3 "help" help
  where
    help (Num i) (Num j) (Num n) = do
      s <- getProSlot i
      if slVitality s < n
        then execError
        else do
          let s' = adjustVitality (-n+) s
          putProSlot i s'
          t <- getProSlot j
          let t' = adjustVitality (+ (n*11`div`10)) t
          putProSlot j t'
          return identity
    help _ _ _= execError

cardCopy = card1 "copy" copy
  where
    copy (Num i) = slField `fmap` getOpSlot i
    copy _ = execError

cardRevive = card1 "revive" revive
  where
    revive (Num i) = do
        s <- getProSlot i
        if dead s
            then putProSlot i (s { slVitality = 1 })
            else return ()
        return identity
    revive _ = execError

cardZombie = card2 "zombie" zombie
  where
    zombie (Num i) x = do
        s <- getOpSlotRev i
        if alive s
          then execError
          else do
            let s' = Slot { slField = x, slVitality = -1 }
            putOpSlotRev i s'
            return identity
    zombie _ _ = execError


