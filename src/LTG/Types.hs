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

module LTG.Types (
  Function(..),
  Value(..),
  Slot(..), alive, dead, zombie,
  Memory(..),
  State(..),
  ExecState(..), Exec(..), apply, execError,
  getProSlot, putProSlot,
  getOpSlotRev, putOpSlotRev,
  getOpSlot,
) where

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector as V




data Function = Func {
    fnName :: String,
    fnArity :: Int,
    fnFilled :: [Value],
    fnPrim :: [Value] -> Exec Value
    }


idFunc :: Function
idFunc = Func "I" 1 [] undefined

data Value = VNum Int | VFunc Function

vname :: Value -> String
vname (VNum i) = show i
vname (VFunc f) = fnName f

vIdentity = VFunc idFunc


data Slot = Slot { slField :: Value, slVitality :: Int }

alive, dead, zombie :: Slot -> Bool
alive s = slVitality s > 0
dead s = slVitality s <= 0
zombie s = slVitality s < 0


type Memory = V.Vector Slot

data State = State {
    stPro :: Memory,
    stOp :: Memory
    }

initState = State initMemory initMemory
  where
    initMemory = V.replicate 256 initSlot
    initSlot = Slot initValue initVitality
    initValue = vIdentity
    initVitality = 10000


data Application = LeftApply | RightApply
data Move = Move
    { mApp :: Application
    , mIndex :: Int
    , mCard :: Value
    }


data ExecState = ES
    { esApplyCount :: Int
    , esState :: State
    , esApplyZombied :: Bool
    }

type Exec = S.State ExecState

execError :: Exec Value
execError = return vIdentity
-- how should this handle short circuiting? Need more complex monad

getProSlot :: Int -> Exec Slot
getProSlot = undefined

putProSlot :: Int -> Slot -> Exec ()
putProSlot = undefined

getOpSlotRev :: Int -> Exec Slot
getOpSlotRev = undefined

putOpSlotRev :: Int -> Slot -> Exec ()
putOpSlotRev = undefined

getOpSlot :: Int -> Exec Slot
getOpSlot = undefined

apply :: Value -> Value -> Exec Value
apply (VNum _) _ = return (vIdentity)
apply (VFunc f) v = let f' = funcAddArg f v in
    if length (fnFilled f') < fnArity f'
        then return (VFunc f')
        else funcExecute f'
  where
    funcAddArg f v = f { fnName = fnName f ++ "(" ++ vname v ++ ")"
                       , fnFilled = fnFilled f ++ [v] }
    funcExecute f = fnPrim f $ fnFilled f
