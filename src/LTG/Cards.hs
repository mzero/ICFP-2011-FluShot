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
    Card,
    cards,
    cardIdentity, cardZero, cardSucc, cardDouble, cardGet, cardPut,
    cardS, cardK, cardInc, cardDec, cardAttack, cardHelp, cardCopy,
    cardRevive, cardZombie,
) where

import LTG.Game


type Card = (String, Value)

cards :: [Card]
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


card :: String -> Value -> Card
card1 :: String -> (Value -> Exec Value) -> Card
card2 :: String -> (Value -> Value -> Exec Value) -> Card
card3 :: String -> (Value -> Value -> Value -> Exec Value) -> Card

card n v = (n, v)
card1 n f = card n $ Func1 n f
card2 n f = card n $ Func2 n f
card3 n f = card n $ Func3 n f


cardIdentity, cardZero, cardSucc, cardDouble, cardGet, cardPut, cardS, cardK
    , cardInc, cardDec, cardAttack, cardHelp, cardCopy, cardRevive, cardZombie
    :: Card

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
    k x _y = return x




adjustVitality ::  Slot -> Int -> Exec Slot
adjustVitality s i = return $
    if alive s
        then s { slVitality = v' }
        else s
  where
    v = slVitality s
    v' = min 65535 $ max 0 $ v + i

cardInc = card1 "inc" inc
  where
    inc (Num i) = do
        s <- getProSlot i
        zombieEffect 1 >>= adjustVitality s >>= putProSlot i
        return identity
    inc _ = execError

cardDec = card1 "dec" dec
  where
    dec (Num i) = do
        s <- getOpSlotRev i
        zombieEffect (-1) >>= adjustVitality s >>= putOpSlotRev i
        return identity
    dec _ = execError


cardAttack = card3 "attack" attack
  where
    attack (Num i) (Num j) (Num n) = do
      s <- getProSlot i
      precondition $ slVitality s >= n
      adjustVitality s (-n) >>= putProSlot i
      t <- getOpSlotRev j
      zombieEffect (-(n*9`div`10)) >>= adjustVitality t >>= putOpSlotRev j
      return identity
    attack _ _ _ = execError

cardHelp = card3 "help" help
  where
    help (Num i) (Num j) (Num n) = do
      s <- getProSlot i
      precondition $ slVitality s >= n
      adjustVitality s (-n) >>= putProSlot i
      t <- getProSlot j
      zombieEffect (n*11`div`10) >>= adjustVitality t >>= putProSlot j
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
            then putProSlot i $ s { slVitality = 1 }
            else return ()
        return identity
    revive _ = execError

cardZombie = card2 "zombie" zombieC
  where
    zombieC (Num i) x = do
        s <- getOpSlotRev i
        precondition $ dead s
        putOpSlotRev i $ Slot { slField = x, slVitality = -1 }
        return identity
    zombieC _ _ = execError


