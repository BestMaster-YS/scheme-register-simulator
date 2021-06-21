{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Control.Monad ( ap, replicateM )
import Control.Applicative
    ( Applicative(liftA2), liftA, liftA3, Alternative(..) )
import Data.Char ( isDigit, isAlpha, isAscii )
import Instruction
    ( Inst(..), InstValue(..), Label(..), Op(..), Reg(..) )

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) } deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) pf pa = pf >>= (`fmap` pa)

instance Monad Parser where
  return a = Parser $ \input -> Just (a, input)
  parseA >>= f = Parser $ \input ->
    let res = runParser parseA input
    in case res of
      Nothing -> Nothing
      Just (a, rest) -> runParser (f a) rest

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) parseA parseB = Parser $ \input ->
    let res = runParser parseA input
    in case res of
      Just a -> Just a
      Nothing -> runParser parseB input

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> Nothing
  (a:rest) -> if p a then Just (a, rest) else Nothing

space :: Parser Char
space = satisfy (==' ')

-- some is one or more, many is 0 or more
spaces :: Parser String
spaces = many space

char :: Parser Char
char = satisfy isAlpha <|> satisfy isDigit <|> char' '-'

char' :: Char -> Parser Char
char' c = satisfy (==c)

word :: Parser String
word = many char

parseSeq :: Parser a -> Parser [a] -> Parser [a]
parseSeq = liftA2 (:)

string :: String -> Parser String
string = foldr (parseSeq . (\c -> satisfy (==c))) (pure "")

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = a *>c <* b

parseDigit :: Parser Char
parseDigit = satisfy isDigit

integer :: Parser Integer
integer = some parseDigit >>= (return . read)

double :: Parser Double
double = liftA3 (\a b c -> a ++ b ++ c) (some parseDigit) (string ".") (some parseDigit) >>= (return . read)

true :: Parser Bool
true =
  (string "#t" >>= const (return True)) <|>
  (string "true" >>= const (return True)) <|>
  (string "True" >>= (return . read))

false :: Parser Bool
false =
  (string "#f" >>= const (return False)) <|>
  (string "false" >>= const (return False)) <|>
  (string "False" >>= (return . read))

nil :: Parser String
nil = string "nil"

isymbol :: Parser String
isymbol = char' '\'' *> word

betweenbracket :: Parser a -> Parser a
betweenbracket parse = satisfy (=='(') *> parse <* satisfy (==')')

parseAssign :: Parser String
parseAssign = string "assign"

-- (reg name) 形式
parseReg :: Parser String
parseReg =  betweenbracket (string "reg" *> spaces *> word)

parseRegInst :: Parser Reg
parseRegInst = parseReg >>= (return . Reg)

parseLabelStr :: Parser String
parseLabelStr = betweenbracket (string "label" *> spaces *> word)

parseComment :: Parser String
parseComment = liftA3 (\_ _ _ -> "") (some (char' ';')) (many (satisfy (\a -> isAscii a && a /= '\n'))) (char' '\n')

-- instValue 由原生数据以及 Reg 和 Label，原生数据都是由 const 包裹住的
parsePrimitiveInstValue :: Parser InstValue
parsePrimitiveInstValue =
  (isymbol >>= (return . ISymbol)) <|>
  (integer >>= (return . IInteger)) <|>
  (double >>= (return . IDouble)) <|>
  (true >>= (return . IBool)) <|>
  (false >>= (return . IBool)) <|>
  (nil >>= const (return INull)) <|>
  (between (string "\"") word (string "\"") >>= (return . IString))

parseConst :: Parser InstValue
parseConst = betweenbracket (string "const" *> spaces *> parsePrimitiveInstValue)

parseInstValue :: Parser InstValue
parseInstValue =
  parseConst <|>
  (parseReg >>= (return . IRegLabel)) <|>
  (parseLabelStr >>= (return . ILabel))

-- assign 指令中 reg 为字符串，非 (reg name) 形式
parseRegString :: Parser Reg
parseRegString = word >>= (return . Reg)

-- (label name) 形式
parseLabel :: Parser Label
parseLabel = parseLabelStr >>= (return . Label)

parseAssignRegPrefix :: Parser Reg
parseAssignRegPrefix = parseAssign *> spaces *> parseRegString

-- op 可能为字母或符号, 使用 asill 判断，但是不能为右括号，不然解析不会停止
parseOpStr :: Parser String
parseOpStr = many (satisfy (\c -> isAscii c && (c /= ')')))

parseOp :: Parser Op
parseOp = betweenbracket (string "op" *> spaces *> parseOpStr) >>= (return . Op)

parseInstValueList :: Parser [InstValue]
parseInstValueList = some (spaces *> parseInstValue)

parsePerformPrefix :: Parser String
parsePerformPrefix = liftA2 (++) (string "perform") spaces

parseTestPrefix :: Parser String
parseTestPrefix = liftA2 (++) (string "test") spaces

parseBranchPrefix :: Parser String
parseBranchPrefix = liftA2 (++) (string "branch") spaces

parseGotoPrefix :: Parser String
parseGotoPrefix = liftA2 (++) (string "goto") spaces

parseSavePerfix :: Parser String
parseSavePerfix = liftA2 (++) (string "save") spaces

parseRestorePerfix :: Parser String
parseRestorePerfix = liftA2 (++) (string "restore") spaces

parseInst :: Parser Inst
parseInst = betweenbracket (
  liftA2 AssignRegReg parseAssignRegPrefix (spaces *> parseRegInst) <|>
  liftA2 AssignRegConst parseAssignRegPrefix (spaces *> parseConst) <|>
  liftA2 AssignRegLabel parseAssignRegPrefix (spaces *> parseLabel) <|>
  liftA3 AssignRegOpResult parseAssignRegPrefix (spaces *> parseOp) parseInstValueList <|>
  liftA2 Perform (parsePerformPrefix *> parseOp) parseInstValueList <|>
  liftA2 Test (parseTestPrefix *> parseOp) parseInstValueList <|>
  fmap Branch (parseBranchPrefix *> parseLabel) <|>
  fmap GotoLabel (parseGotoPrefix *> parseLabel) <|>
  fmap GotoReg (parseGotoPrefix *> parseRegInst) <|>
  fmap SaveReg (parseSavePerfix *> parseRegString) <|>
  fmap RestoreReg (parseRestorePerfix *> parseRegString)
  )

parseFile :: String -> [Either Label Inst]
parseFile content = map (\input ->
  case runParser parseLine input of
    Nothing -> error ("parseLine error: " ++ input)
    Just a -> fst a
  )
  (filter (/="")  (lines content))

parseLine :: Parser (Either Label Inst)
parseLine =
  (parseInst >>= (return . Right)) <|>
  (word >>= (return . (Left . Label)))


testARR :: Parser Inst
testARR = betweenbracket (liftA2 AssignRegReg parseAssignRegPrefix parseRegInst)

testGL :: Parser Inst
testGL = betweenbracket (fmap GotoLabel (parseGotoPrefix *> parseLabel))

testAROP :: Parser Inst
testAROP = betweenbracket (liftA3 AssignRegOpResult parseAssignRegPrefix (spaces *> parseOp) parseInstValueList)

testTOP :: Parser Inst
testTOP = betweenbracket (liftA2 Test (parseTestPrefix *> parseOp) parseInstValueList)

testParseFile :: IO ()
testParseFile = do
  content <- readFile "test1.hcm"
  let r = parseFile content
  print r
  return ()
