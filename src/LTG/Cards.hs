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

) where

import LTG.Types



type Card = Value


buildFuncCard :: String -> Int -> ([Value] -> Exec Value) -> Card
buildFuncCard name arity f = VFunc $ Func name arity [] f


cardIdentity = buildFuncCard "I" 1 $ (return . head)

cardZero = VNum 0

cardSucc = buildFuncCard "succ" 1 (numFunc (+1))
cardDouble = buildFuncCard "dbl" 1 (numFunc (*2))


numFunc :: (Int -> Int) -> [Value] -> Exec Value
numFunc f [(VNum i)] = return (VNum i')
  where
    i' = min 65535 (max 0 (f i))
numFunc _ _ = execError


cardGet = buildFuncCard "get" 1 get
  where
    get [VNum i] = do
        s <- getProSlot i
        if alive s
          then return (slField s)
          else execError
    get _ = execError

cardPut = buildFuncCard "put" 1  put
  where
    put _ = return cardIdentity

cardS = buildFuncCard "S" 3 s
  where
    s [f,g,x] = do
      h <- apply f x
      y <- apply g x
      apply h y
    s _ = execError

cardK = buildFuncCard "K" 2  k
  where
    k [x,y] = return x
    k _ = execError

cardInc = buildFuncCard "inc" 1 inc
  where
    inc [VNum i] = do
        s <- getProSlot i
        let s' = adjustVitality (+1) s
        putProSlot i s'
        return cardIdentity
    inc _ = execError

cardDec = buildFuncCard "dec" 1 dec
  where
    dec [VNum i] = do
        s <- getOpSlotRev i
        let s' = adjustVitality (-1+) s
        putOpSlotRev i s'
        return cardIdentity
    dec _ = execError

adjustVitality :: (Int -> Int) -> Slot -> Slot
adjustVitality f s = s { slVitality = v' }
  where
    v = slVitality s
    v' = if v > 0 then min 65535 (max 0 (f v)) else v

cardAttack = buildFuncCard "attack" 3 attack
  where
    attack [VNum i, VNum j, VNum n] = do
      s <- getProSlot i
      if slVitality s < n
        then execError
        else do
          let s' = adjustVitality (subtract n) s
          putProSlot i s'
          t <- getOpSlotRev j
          let t' = adjustVitality (subtract (n*9`div`10)) t
          putOpSlotRev j t'
          return cardIdentity
    attack _ = execError

cardHelp = buildFuncCard "help" 3 help
  where
    help [VNum i, VNum j, VNum n] = do
      s <- getProSlot i
      if slVitality s < n
        then execError
        else do
          let s' = adjustVitality (-n+) s
          putProSlot i s'
          t <- getProSlot j
          let t' = adjustVitality (+ (n*11`div`10)) t
          putProSlot j t'
          return cardIdentity
    help _ = execError

cardCopy = buildFuncCard "copy" 1 copy
  where
    copy [VNum i] = slField `fmap` getOpSlot i
    copy _ = execError

cardZombie = buildFuncCard "zombie" 2 zombie
  where
    zombie [VNum i, x] = do
        s <- getOpSlotRev i
        if alive s
          then execError
          else do
            let s' = Slot { slField = x, slVitality = -1 }
            putOpSlotRev i s'
            return cardIdentity
    zombie _ = execError


