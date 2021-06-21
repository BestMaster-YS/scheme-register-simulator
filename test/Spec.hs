import Machine
import Data.Map
import Control.Monad.State
import Instruction
import Assemble

machine = initMachine

testInst :: [Either Label Inst]
testInst = [
  Left (Label "test-b"),
  Right (Test (Op "=") [IRegLabel "b",IInteger 0]),
  Right (Branch (Label "gcd-done")),
  Right (AssignRegOpResult (Reg "t") (Op "rem") [IRegLabel "a",IRegLabel "b"]),
  Right (AssignRegReg (Reg "a") (Reg "b")),
  Right (AssignRegReg (Reg "b") (Reg "t")),
  Left (Label "gcd-done")
  ]

testEnv = snd (extractLabels testInst)

testUpdateEnvs :: State Machine ()
testUpdateEnvs = do
  updateEnvS testEnv
  -- updateInstsByLabel "test-b"


main :: IO ()
main = do
  let m = env (execState testUpdateEnvs machine)
  print testEnv
  print m







