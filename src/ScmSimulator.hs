module ScmSimulator (run) where

import Control.Monad.State ( runState, StateT )
import System.Environment ( getArgs )
import Parser ( parseWholeFile, runParser, ParserResult(..) )
import Assemble ( assemble, extractLabels, assembleS )
import Instruction (InstValue(..), Inst(..), Label(..), Reg(..), Op(..) )
import Machine (lookupRegister, initMachine, allocRegister, getFlag, Machine(..) )

testInst :: [Either Label Inst]
testInst = [
    Right (Test (Op "=") [IRegLabel "b",IInteger 0]),
    Right (Branch (Label "gcd-done")),
    Right (AssignRegOpResult (Reg "t") (Op "rem") [IRegLabel "a",IRegLabel "b"]),
    Right (AssignRegReg (Reg "a") (Reg "b")),
    Right (AssignRegReg (Reg "b") (Reg "t")),
    Left (Label "gcd-done")
  ]

run :: IO ()
run = do
  files <- getArgs
  case files of
    [] -> putStrLn "Please run this program with filename be the arguments"
    (filename:_) -> do
      content <- readFile filename
      let result = runParser parseWholeFile content
      case result of
        ParseResult list _ -> do
          print list
          let m1 = allocRegister "a" (IInteger 206) initMachine
          let m2 = allocRegister "b" (IInteger 40) m1
          let (_, m3) = runState (assembleS list) m2
          print (lookupRegister "a" m3)
          print (lookupRegister "b" m3)
          putStrLn "done"
        error@(ParseError e a) -> print error
        part@ParsePart -> print part
