{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import System.IO.Unsafe
import Data.Foldable
import Control.Monad ( ap, replicateM )
import Control.Applicative
    ( Applicative(liftA2), liftA, liftA3, Alternative(..) )
import Data.Char ( isDigit, isAlpha, isAscii )
import Instruction
    ( Inst(..), InstValue(..), Label(..), Op(..), Reg(..) )

-- 重写 Parser , Maybe 不满足现在的需求

type Input = String
unknown :: String
unknown = "Unknown"
data ParserResult a = ParseResult a Input
                    -- 以下皆为错误信息，但是错误含义不一样
                    | ParseError
                      Input -- 期待输入，有时候可能没有
                      Input -- 实际输入
                    | ParsePart -- parse part

instance (Show a) => Show (ParserResult a) where
  show (ParseResult a i) = "Result: " ++ show a ++ ", rest: " ++ i
  show (ParseError e a) = "Error: expect " ++ show e ++ ", actual " ++ show a
  show ParsePart = "input uncompleted"

instance Functor ParserResult where
  fmap f (ParseResult a i) = ParseResult (f a) i
  fmap f (ParseError e a) = ParseError e a
  fmap f ParsePart = ParsePart

newtype Parser a = Parser { runParser :: String -> ParserResult a } deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) pf pa = pf >>= (`fmap` pa)

instance Monad Parser where
  return a = Parser $ \input -> ParseResult a input
  parseA >>= f = Parser $ \input ->
    let res = runParser parseA input
    in case res of
      ParseResult a next -> runParser (f a) next
      ParseError e a -> ParseError e a
      ParsePart -> ParsePart

instance Alternative Parser where
  empty = Parser $ const ParsePart
  (<|>) parseA parseB = Parser $ \input ->
    let res = runParser parseA input
    in case res of
      ParseResult a next -> ParseResult a next
      ParseError e a -> runParser parseB input
      ParsePart -> runParser parseB input

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy error p = Parser $ \case
  [] -> ParsePart
  (a:rest) -> if p a then ParseResult a rest else ParseError error (show a)

space :: Parser Char
space = satisfy "space" (==' ')

-- some is one or more, many is 0 or more
spaces :: Parser String
spaces = many space

spacesOrLineBreak :: Parser String
spacesOrLineBreak = many (space <|> char' '\n')

char :: Parser Char
char = satisfy "alpha, digit, -, _" (\c -> isAlpha c || isDigit c || c == '-' || c == '_')

char' :: Char -> Parser Char
char' c = satisfy (show c) (==c)

word :: Parser String
word = some char

parseSeq :: Parser a -> Parser [a] -> Parser [a]
parseSeq = liftA2 (:)

string :: String -> Parser String
string = foldr (parseSeq . (\c -> satisfy (show c) (==c))) (pure "")

between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a *> b <* c

parseDigit :: Parser Char
parseDigit = satisfy "digit" isDigit

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
betweenbracket parse = satisfy "(" (=='(') *> parse <* satisfy ")" (==')')

parseAssign :: Parser String
parseAssign = string "assign"

{- |

>>> runParser parseReg "(reg \n name)"
Just ("name","")

-}
-- (reg name) 形式
parseReg :: Parser String
parseReg =  betweenbracket (string "reg" *> spacesOrLineBreak *> word)

parseRegInst :: Parser Reg
parseRegInst = parseReg >>= (return . Reg)

-- (label label-name)
parseLabelStr :: Parser String
parseLabelStr = betweenbracket (string "label" *> spacesOrLineBreak *> word)


{- |
>>> runParser parseComment ";; test comment \n"
Just ("","")
-}
parseComment :: Parser String
parseComment =
  liftA3 (\_ _ _ -> "")
    (some (char' ';'))
    (many (satisfy "asicc && not \\n" (\a -> isAscii a && a /= '\n')))
    (char' '\n')

-- instValue 由原生数据以及 Reg 和 Label，原生数据都是由 const 包裹住的

parsePrimitiveString :: Parser String
parsePrimitiveString = some (satisfy "ascii && not \"" (\a -> isAscii a && a /= '\"'))

parsePrimitiveInstValue :: Parser InstValue
parsePrimitiveInstValue =
  (isymbol >>= (return . ISymbol)) <|>
  (integer >>= (return . IInteger)) <|>
  (double >>= (return . IDouble)) <|>
  (true >>= (return . IBool)) <|>
  (false >>= (return . IBool)) <|>
  (nil >>= const (return INull)) <|>
  (between (char' '\"') parsePrimitiveString (char' '\"') >>= (return . IString)) <|>
  (word >>= (return . IVarible))

parseConst :: Parser InstValue
parseConst = betweenbracket (string "const" *> spacesOrLineBreak *> parsePrimitiveInstValue)

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
parseAssignRegPrefix = parseAssign *> spacesOrLineBreak *> parseRegString

-- op 可能为字母或符号, 使用 asill 判断，但是不能为右括号，不然解析不会停止
parseOpStr :: Parser String
parseOpStr = many (satisfy "ascii && not )" (\c -> isAscii c && (c /= ')')))

parseOp :: Parser Op
parseOp = betweenbracket (string "op" *> spacesOrLineBreak *> parseOpStr) >>= (return . Op)

parseInstValueList :: Parser [InstValue]
parseInstValueList = many (spacesOrLineBreak *> parseInstValue)

parsePerformPrefix :: Parser String
parsePerformPrefix = liftA2 (++) (string "perform") spacesOrLineBreak

parseTestPrefix :: Parser String
parseTestPrefix = liftA2 (++) (string "test") spacesOrLineBreak

parseBranchPrefix :: Parser String
parseBranchPrefix = liftA2 (++) (string "branch") spacesOrLineBreak

parseGotoPrefix :: Parser String
parseGotoPrefix = liftA2 (++) (string "goto") spacesOrLineBreak

parseSavePerfix :: Parser String
parseSavePerfix = liftA2 (++) (string "save") spacesOrLineBreak

parseRestorePerfix :: Parser String
parseRestorePerfix = liftA2 (++) (string "restore") spacesOrLineBreak

parseInst :: Parser Inst
parseInst = betweenbracket (
  liftA2 AssignRegReg parseAssignRegPrefix (spacesOrLineBreak *> parseRegInst) <|>
  liftA2 AssignRegConst parseAssignRegPrefix (spacesOrLineBreak *> parseConst) <|>
  liftA2 AssignRegLabel parseAssignRegPrefix (spacesOrLineBreak *> parseLabel) <|>
  liftA3 AssignRegOpResult parseAssignRegPrefix (spacesOrLineBreak *> parseOp) parseInstValueList <|>
  liftA2 Perform (parsePerformPrefix *> parseOp) parseInstValueList <|>
  liftA2 Test (parseTestPrefix *> parseOp) parseInstValueList <|>
  fmap Branch (parseBranchPrefix *> parseLabel) <|>
  fmap GotoLabel (parseGotoPrefix *> parseLabel) <|>
  fmap GotoReg (parseGotoPrefix *> parseRegInst) <|>
  fmap SaveReg (parseSavePerfix *> parseRegString) <|>
  fmap RestoreReg (parseRestorePerfix *> parseRegString)
  ) <* spacesOrLineBreak

parseWholeFile :: Parser [Either Label Inst]
parseWholeFile =
  liftA2 (:) parseWholeInst parseWholeFile <|>
  liftA2 (\_ x -> x) parseComment (spacesOrLineBreak *> parseWholeFile) <|>
  return []

parseWholeInst :: Parser (Either Label Inst)
parseWholeInst =
  (parseInst >>= (return . Right)) <|>
  ((word >>= (return . (Left . Label))) <* spacesOrLineBreak)

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
  content <- readFile "scheme.hcm"
  let r = runParser parseWholeFile content
  case r of
    ParseError e a -> print ("parse error: expect: " ++ e ++ ", actual: " ++ a)
    ParseResult list next -> do
      traverse_ print list
      print next
      return ()
    a@ParsePart -> do
      print a
      return ()

