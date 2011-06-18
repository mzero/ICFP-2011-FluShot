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
  Value(..), valueName, identity,
  Slot(..), alive, dead, zombie,
  ExecState(..), Exec(..), apply, execError,
  getProSlot, putProSlot,
  getOpSlotRev, putOpSlotRev,
  getOpSlot,
) where

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector as V




data Value = Num Int
           | Func1 String (Value -> Exec Value)
           | Func2 String (Value -> Value -> Exec Value)
           | Func3 String (Value -> Value -> Value -> Exec Value)

valueName :: Value -> String
valueName (Num i) = show i
valueName (Func1 n _) = n
valueName (Func2 n _) = n
valueName (Func3 n _) = n

identity = Func1 "I" return


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
    initValue = identity
    initVitality = 10000



data ExecState = ES
    { esApplyCount :: Int
    , esState :: State
    , esApplyZombied :: Bool
    }

type Exec = S.State ExecState

execError :: Exec Value
execError = return identity
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
-- need to guard on application count here
apply (Num _) _ = execError
apply (Func3 n f) v = return $ Func2 (applyName n v) (f v)
apply (Func2 n f) v = return $ Func1 (applyName n v) (f v)
apply (Func1 _ f) v = f v

applyName :: String -> Value -> String
applyName s v = s ++ "(" ++ valueName v ++ ")"



data Application = LeftApply | RightApply
data Move = Move
    { mApp :: Application
    , mIndex :: Int
    , mCard :: Value
    }
