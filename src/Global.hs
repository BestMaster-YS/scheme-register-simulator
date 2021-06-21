module Global where

import qualified Data.Map as Map
import Instruction ( InstValue(..) )

globalOperatorFunctions :: Map.Map String ([InstValue] -> InstValue)
globalOperatorFunctions = Map.empty

plus :: [InstValue] -> InstValue
plus is
  | null is = IInteger 0
  | length is == 1 = head is
  | otherwise =
    let (i:j:rest) = is
    in case i of
        IInteger v ->
          case j of
            IInteger v' -> plus (IInteger (v + v'):rest)
            IDouble d'  -> plus (IDouble (d' + fromInteger v):rest)
            _ -> error ("\"+\" expcet Integer or Double arguments but actally is " ++ show j)
        IDouble v ->
          case j of
            IInteger v' -> plus (IDouble (v + fromInteger v'):rest)
            IDouble d'  -> plus (IDouble (d' + v):rest)
            _ -> error ("\"+\" expcet Integer or Double arguments but actally is " ++ show j)
        _ -> error ("\"+\" expcet Integer or Double arguments but actally is " ++ show i)

eqInst :: [InstValue] -> InstValue
eqInst is = if length is == 2
            then
              let (i:j:_) = is
              in if i == j then IBool True else IBool False
            else error "\"+\" need two arguments"

remInst :: [InstValue] -> InstValue
remInst is = if length is == 2
            then
              let (i:j:_) = is
              in case i of
                IInteger v ->
                  case j of
                    IInteger v' ->  IInteger (rem v v')
                    _ -> error ("\"+\" expcet two Integer arguments but actally is " ++ show j)
                _ -> error ("\"+\" expcet two Integer arguments but actally is " ++ show i)
            else error "\"+\" need two arguments"


baseFunctions :: [(String, [InstValue] -> InstValue)]
baseFunctions = [
  ("+", \intses -> if length intses <= 1 then error "\"+\" need two arguments at least" else plus intses),
  ("=", eqInst),
  ("rem", remInst)
  ]

initBaseFns :: Map.Map String ([InstValue] -> InstValue)
initBaseFns = foldr (\(k, f) m -> Map.insert k f m) globalOperatorFunctions baseFunctions
