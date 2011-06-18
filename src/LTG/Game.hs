-----------------------------------------------------------------------------
--
-- Module      :  LTG.Types
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

module LTG.Game (
  Value(..), valueName, identity,
  Slot(..), alive, dead, zombie,
  State, initState,
  Exec,
  execute,
  execError, precondition,
  getProSlot, putProSlot,
  getOpSlotRev, putOpSlotRev,
  getOpSlot,
  slotRange, proSlots,
  apply,
  zombify,
  zombieEffect,
  switchSides,
) where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM




data Value = Num !Int
           | Func1 !String (Value -> Exec Value)
           | Func2 !String (Value -> Value -> Exec Value)
           | Func3 !String (Value -> Value -> Value -> Exec Value)

valueName :: Value -> String
valueName (Num i) = show i
valueName (Func1 n _) = n
valueName (Func2 n _) = n
valueName (Func3 n _) = n

identity = Func1 "I" return


data Slot = Slot { slField :: !Value, slVitality :: !Int }

alive, dead, zombie :: Slot -> Bool
alive s = slVitality s > 0
dead s = slVitality s <= 0
zombie s = slVitality s < 0


type Memory = V.Vector Slot

data State = State {
    stPro :: !Memory,
    stOp :: !Memory
    }

initState = State initMemory initMemory
  where
    initMemory = V.replicate 256 initSlot
    initSlot = Slot initValue initVitality
    initValue = identity
    initVitality = 10000

proSlots :: State -> [Slot]
proSlots = V.toList . stPro

data ExecState = ES
    { esApplyCount :: !Int
    , esState :: !State
    , esZombied :: !Bool
    }

newtype Exec a = Exec { runExec :: ExecState -> (Maybe a, ExecState) }

instance Functor Exec where
    f `fmap` m = Exec (\e -> let (r, e') = runExec m e in (f `fmap` r, e'))

instance Monad Exec where
    return a = Exec (\e -> (Just a, e))
    m >>= g = Exec (\e -> case runExec m e of
                            (Nothing, e') -> (Nothing, e')
                            (Just a, e') -> runExec (g a) e')
    fail msg = Exec (\e -> (Nothing, e))

instance MonadPlus Exec where
    mzero = fail "mzero"
    a `mplus` b = Exec (\e -> case runExec a e of
                            (Nothing, _) -> runExec b e
                            k@(Just a, e') -> k)

execute :: Exec a -> State -> State
execute m s = esState . snd $ runExec m baseState
  where baseState = ES { esApplyCount = 0, esState = s, esZombied = False }


execError :: Exec a
execError = fail undefined

precondition :: Bool -> Exec ()
precondition b = if b then return () else execError


get :: Exec ExecState
get = Exec (\e -> (Just e, e))

put :: ExecState -> Exec ()
put e = Exec (\_ -> (Just (), e))

modify :: (ExecState -> ExecState) -> Exec ()
modify f = get >>= put . f

modifyState :: (State -> State) -> Exec ()
modifyState f = modify (\e -> e { esState = f (esState e) })


modifyProMemory, modifyOpMemory :: (Memory -> Memory) -> Exec ()
modifyProMemory f = modifyState (\s -> s { stPro = f (stPro s) })
modifyOpMemory f = modifyState (\s -> s { stOp = f (stOp s) })

validSlot :: (State -> Memory) -> Int -> Exec Slot
validSlot t i = do
    m <- (t.esState) `fmap` get
    precondition (0 <= i  &&  i < V.length m)
    return $ m V.! i

getProSlot :: Int -> Exec Slot
getProSlot = validSlot stPro

putProSlot :: Int -> Slot -> Exec ()
putProSlot i s = do
    validSlot stPro i
    modifyProMemory (V.modify (\v -> VM.write v i s))


getOpSlotRev :: Int -> Exec Slot
getOpSlotRev i = validSlot stOp (255 - i)

putOpSlotRev :: Int -> Slot -> Exec ()
putOpSlotRev i s = let i' = 255 - i in do
    validSlot stOp i'
    modifyOpMemory (V.modify (\v -> VM.write v i' s))

getOpSlot :: Int -> Exec Slot
getOpSlot = validSlot stOp


slotRange :: Exec (Int, Int)
slotRange = do
    m <- (stPro.esState) `fmap` get
    return (0, V.length m)

apply :: Value -> Value -> Exec Value
apply f v = do
    e <- get
    let ac = esApplyCount e
    precondition (ac < 1000)
    put e { esApplyCount = ac + 1 }
    apply' f v
  where
    apply' (Num _) _ = execError
    apply' (Func3 n f) v = return $ Func2 (applyName n v) (f v)
    apply' (Func2 n f) v = return $ Func1 (applyName n v) (f v)
    apply' (Func1 _ f) v = f v

    applyName :: String -> Value -> String
    applyName s v = s ++ "(" ++ valueName v ++ ")"


zombify :: Exec a -> Exec a
zombify m = do
    modify (\e -> e { esZombied = True })
    a <- m
    modify (\e -> e { esZombied = False })
    return a


zombieEffect ::  Int -> Exec Int
zombieEffect i = do
    z <- esZombied `fmap` get
    return $ if z then (-i) else i

switchSides :: Exec ()
switchSides = modifyState switch
  where
    switch s = s { stPro = stOp s, stOp = stPro s }

