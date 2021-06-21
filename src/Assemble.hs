{-# LANGUAGE LambdaCase #-}
module Assemble where

import qualified Data.Bifunctor as BF
import qualified Data.Map as Map
import Control.Monad.State ( StateT, State, state, runState, runStateT )
import Control.Monad.Reader ( ReaderT(..), runReaderT, Reader, reader )
import Instruction
    ( Inst(..),
      InstValue(..),
      Label(..),
      Op(Op),
      Reg(Reg) )
import Machine
    ( Machine,
      updateFlag,
      updateFlagS,
      getFlag,
      getFlagS,
      allocRegister,
      allocRegisterS,
      lookupRegister,
      lookupRegisterS,
      push,
      pushS,
      pop,
      popS,
      nextInstS,
      updateInstS,
      updateEnvS,
      updateInstsByLabel,
      getEnvS,
      Env
    )
import Global ( initBaseFns )

-- Assemble 程序就是将指令转换为对应作用的可执行函数列表
-- 为什么要先将指令变为函数，而不是当指令真正执行时才进行解析指令，并执行？
-- 因为可能一些 label 下指令会经常重复解析执行（如循环内的指令），提前解析好，可以提高性，再次使用时直接调用对应的方法
-- 就可以了

-- 从全局看：Instruction 指令的执行，都是对 Machine 的改变，可以将 Instruction 的执行函数看做为 State
-- 还有就是遇到 label 和 goto 指令时，需要进行去读取对应 label 的指令，所以需要 Reader 存储 Map Label [Instruction] 全局变量
-- 再讨论 label 与 Instruction 的关系，在 SICP 中，label 对应的指令包括了 label 下所有的指令
-- 我们也先采用这种方式

-- 所以 assemble 函数的作用就是接受开始的指令列表，返回一个 State Monad Transformer
-- 该 State Monad (称为 s1) 的维护的状态为一个 [State Monad] (s2)，s2 维护的状态为 Machine ，每一个 s2 都是
-- 对应 Instruction 解析后的执行函数

-- env 作为保存 label 与 指令序列的环境


-- extractLabels
extractLabels :: [Either Label Inst] -> ([Inst], Env)
extractLabels = foldr (
  \case
    Left (Label label) -> \res -> BF.second (Map.insert label (fst res)) res
    Right inst -> BF.first (inst :)
  ) ([], Map.empty)

-- 将 IRegLabel 转换为其他 InstValue
convertRegToInstValue :: Machine -> InstValue -> InstValue
convertRegToInstValue m =
  \case
    IRegLabel reg -> lookupRegister reg m
    a -> a

runFuncWithInsts :: ([InstValue] -> InstValue) -> Machine -> [InstValue] -> InstValue
runFuncWithInsts f m ins = f (map (convertRegToInstValue m) ins)

-- handleInst 处理指令，若是为 label 跳转指令时，需要返回新的指令序列和 machine
handleInst :: Inst -> Env -> Machine -> (Maybe [Inst], Machine)
handleInst inst env m =
  case inst of
    AssignRegReg (Reg k1) (Reg k2) ->
      let v = lookupRegister k2 m
      in (Nothing, allocRegister k1 v m)
    AssignRegConst (Reg k) val -> (Nothing, allocRegister k val m)
    AssignRegOpResult (Reg k) (Op op) insts ->
      let mf = Map.lookup op initBaseFns
      in case mf of
        Nothing -> error ("Can't find Operator: " ++ op)
        Just f -> (Nothing, allocRegister k (runFuncWithInsts f m insts) m)
    AssignRegLabel (Reg k) (Label label) ->
      let regLabel = IRegLabel label
      in (Nothing, allocRegister k regLabel m)
    Perform (Op op) insts ->
      let mf = Map.lookup op initBaseFns
      in case mf of
        Nothing -> error ("Can't find Operator: " ++ op)
        Just f ->
          -- perform 并不会影响到 Machine 状态
          let _ = runFuncWithInsts f m insts
          in (Nothing, m)
    Test (Op op) insts ->
      let mf = Map.lookup op initBaseFns
      in case mf of
        Nothing -> error ("Can't find Operator: " ++ op)
        Just f ->
          case runFuncWithInsts f m insts of
            (IBool b) -> (Nothing, updateFlag b m)
            _ -> error (show op ++ " Operator could not return True/False")
    Branch (Label label) ->
      let cond = getFlag m
      in if cond
          then (Map.lookup label env, m)
          else (Nothing, m)
    GotoLabel (Label label) -> (Map.lookup label env, m)
    GotoReg (Reg value) ->
      case lookupRegister value m of
        ILabel label -> (Map.lookup label env, m)
        _ -> error ("Can't use GotoReg Instruction with value are't RegLabel, Reg key: " ++ show value)
    SaveReg (Reg k) ->
      let v = lookupRegister k m
      in (Nothing, push v m)
    RestoreReg (Reg k) ->
      let (v, m') = pop m
      in (Nothing, allocRegister k v m')

runInst :: ReaderT Env (State ([Inst], Machine)) ()
runInst = ReaderT (\env ->
  state $ \(ins,m) ->
    case ins of
      [] -> return ([], m)
      (inst:rest) ->
        let (next, m') = handleInst inst env m
        in case next of
          Nothing -> return (rest, m')
          Just newInsts -> return (newInsts, m')
  )

execute :: ([Inst], Machine) -> Env -> Machine
execute initS env =
  let (_, next@(insts, m)) = runState (runReaderT runInst env) initS
  in case insts of
    [] -> m
    rest -> execute next env

assemble :: [Either Label Inst] -> Machine -> Machine
assemble list m =
  let (inst, env) = extractLabels list
  in execute (inst, m) env

-- 全部 State 化

convertRegToInstValueS :: InstValue -> State Machine InstValue
convertRegToInstValueS =
  \case
    IRegLabel reg -> do
      lookupRegisterS reg
    a -> return a

runFuncWithInstsS :: ([InstValue] -> InstValue) -> [InstValue] -> State Machine InstValue
runFuncWithInstsS f ins = do
  newInsts <- traverse convertRegToInstValueS ins
  return (f newInsts)

handleInstS :: Inst -> State Machine (Maybe String)
handleInstS =
  \case
    AssignRegReg (Reg k1) (Reg k2) -> do
      v <- lookupRegisterS k2
      allocRegisterS k1 v
      return Nothing
    AssignRegConst (Reg k) val -> do
      allocRegisterS k val
      return Nothing
    AssignRegOpResult (Reg k) (Op op) insts ->
      let mf = Map.lookup op initBaseFns
      in case mf of
        Nothing -> error ("Can't find Operator: " ++ op)
        Just f -> do
          v <- runFuncWithInstsS f insts
          allocRegisterS k v
          return Nothing
    AssignRegLabel (Reg k) (Label label) ->
      let regLabel = IRegLabel label
      in do
        allocRegisterS k regLabel
        return Nothing
    Perform (Op op) insts ->
      let mf = Map.lookup op initBaseFns
      in case mf of
        Nothing -> error ("Can't find Operator: " ++ op)
        Just f -> do
          -- perform 并不会影响到 Machine 状态
          runFuncWithInstsS f insts
          return Nothing
    Test (Op op) insts ->
      let mf = Map.lookup op initBaseFns
      in case mf of
        Nothing -> error ("Can't find Operator: " ++ op)
        Just f -> do
          v <- runFuncWithInstsS f insts
          case v of
            (IBool b) -> do
              updateFlagS b
              return Nothing
            _ -> error (show op ++ " Operator could not return True/False")
    Branch (Label label) -> do
      cond <- getFlagS
      if cond
        then return (Just label)
        else do
          return Nothing
    GotoLabel (Label label) -> return (Just label)
    GotoReg (Reg value) -> do
      v <- lookupRegisterS value
      case v of
        ILabel label -> return (Just label)
        _ -> error ("Can't use GotoReg Instruction with value are't RegLabel, Reg key: " ++ show value)
    SaveReg (Reg k) -> do
      v <- lookupRegisterS k
      pushS v
      return Nothing
    RestoreReg (Reg k) -> do
      v <- popS
      allocRegisterS k v
      return Nothing

runInstS :: State Machine ()
runInstS = do
  mn <- nextInstS
  case mn of
    Nothing -> return ()
    Just (inst, nexts) ->
      do
        mlabel <- handleInstS inst
        case mlabel of
          Nothing -> do
            updateInstS nexts
            runInstS
          Just label -> do
            updateInstsByLabel label
            runInstS

assembleS :: [Either Label Inst] -> State Machine ()
assembleS list =
  let (inst, env) = extractLabels list
  in do
    updateInstS inst
    updateEnvS env
    runInstS


