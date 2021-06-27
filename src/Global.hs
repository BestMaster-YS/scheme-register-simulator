module Global where

import System.IO.Unsafe
import qualified Data.Map as Map
import Instruction ( InstValue(..) )
import Language.IO ( readExp, unsafePrompt )
import Language.Syntax
import Language.Parser
import Parser

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

readSchemeExp :: [InstValue] -> InstValue
readSchemeExp _ =
  let exp = unsafePerformIO (readExp "")
  in IString exp

normalPrint :: [InstValue] -> InstValue
normalPrint (v:_) =
  let _ = unsafePrompt (show v)
  in INull

schemeSyntaxpredicate :: (SchemeContent -> Bool) -> [InstValue] -> InstValue
schemeSyntaxpredicate p (v:_) =
  case v of
    IString s ->
      case runParser parseScheme s of
        -- readExp 已经进行了错误处理，这里就不重复处理
        ParseResult a _ -> if p a then IBool True else IBool False
    _ -> error "scheme syntax runtime error"


baseFunctions :: [(String, [InstValue] -> InstValue)]
baseFunctions = [
  ("+", \intses -> if length intses <= 1 then error "\"+\" need two arguments at least" else plus intses),
  ("=", eqInst),
  ("rem", remInst),
  ("read", readSchemeExp),
  ("prompt-for-input", normalPrint),
  ("user-print", normalPrint),
  ("self-evaluating?", schemeSyntaxpredicate selfEvaluating),
  ("variable?", schemeSyntaxpredicate isVarible),
  ("quoted?", schemeSyntaxpredicate isSymbol),
  ("assignment?", schemeSyntaxpredicate isAssignment),
  ("definition?", schemeSyntaxpredicate isDefinition),
  ("if?", schemeSyntaxpredicate isIf),
  ("lambda?", schemeSyntaxpredicate isLambda),
  ("begin?", schemeSyntaxpredicate isBegin),
  ("application?", schemeSyntaxpredicate isApplication)
  ]

initBaseFns :: Map.Map String ([InstValue] -> InstValue)
initBaseFns = foldr (\(k, f) m -> Map.insert k f m) globalOperatorFunctions baseFunctions
