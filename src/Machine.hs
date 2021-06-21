{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Machine where

import Control.Monad.State (State, runState, state)
import qualified Data.Map as Map
import Instruction (Inst, InstValue)

type MachineStack = [InstValue]
type Env = Map.Map String [Inst]

data Machine = Machine
  {
    stack :: MachineStack,
    flag  :: Bool,
    regs  :: Map.Map String InstValue,
    insts :: [Inst],
    env   :: Env
  }

initMachine :: Machine
initMachine = Machine
  {
    stack = [],
    flag  = False,
    regs  = Map.empty,
    insts = [],
    env   = Map.empty
  }

updateFlag :: Bool -> Machine -> Machine
updateFlag v Machine {..} = Machine
  {
    stack,
    flag = v,
    regs,
    insts,
    env
  }

updateFlagS :: Bool -> State Machine ()
updateFlagS b = state (\m -> ((), updateFlag b m))

getFlag :: Machine -> Bool
getFlag Machine {..} = flag

getFlagS :: State Machine Bool
getFlagS = state (\m -> (getFlag m, m))

allocRegister :: String -> InstValue -> Machine -> Machine
allocRegister k v Machine {..} = Machine
  {
    stack,
    flag,
    regs = Map.insert k v regs,
    insts,
    env
  }

allocRegisterS :: String -> InstValue -> State Machine ()
allocRegisterS k v = state (\m -> ((), allocRegister k v m))

lookupRegister :: String -> Machine -> InstValue
lookupRegister k m =
  case Map.lookup k (regs m) of
    Nothing -> error ("Unknown register: " ++ k)
    Just v  -> v

lookupRegisterS :: String -> State Machine InstValue
lookupRegisterS k = state (\m -> (lookupRegister k m, m))

push :: InstValue -> Machine -> Machine
push v Machine{..} = Machine
  {
    stack = v : stack,
    flag,
    regs,
    insts,
    env
  }

pushS :: InstValue -> State Machine ()
pushS v = state (\m -> ((), push v m))

pop :: Machine -> (InstValue, Machine)
pop Machine{..} =
  case stack of
    [] -> error "Empty stack --- POP"
    (v:vs) -> (v, Machine { stack = vs, flag, regs, insts, env })

popS :: State Machine InstValue
popS = state (\m@Machine{..} ->
  case stack of
    [] -> error "Empty stack --- POP"
    (v:vs) -> (v, Machine { stack = vs, flag, regs, insts, env })
  )

nextInstS :: State Machine (Maybe (Inst, [Inst]))
nextInstS = state (\m@Machine{..} ->
    case insts of
      [] -> (Nothing, m)
      (inst:next) -> (Just (inst, next), m)
  )

updateInstS :: [Inst] -> State Machine ()
updateInstS is = state (\Machine{..} -> ((), Machine{
    insts = is,
    flag,
    regs,
    stack,
    env
  }))

updateEnvS :: Env -> State Machine ()
updateEnvS newE = state (\Machine{..} -> ((), Machine{
    insts,
    flag,
    regs,
    stack,
    env = newE
  }))

getEnvS :: State Machine Env
getEnvS = state (\m@Machine{..} -> (env, m))

updateInstsByLabel :: String -> State Machine ()
updateInstsByLabel label = do
  env <- getEnvS
  let m = Map.lookup label env
  case m of
    Nothing -> error ("Can't find the label: " ++ label)
    Just is -> do
      updateInstS is









