module Language.Parser where

import Control.Applicative
import Parser
import Language.Syntax

-- scheme 语法解析器

parseSchemeDouble :: Parser SchemeContent
parseSchemeDouble = double >>= (return . SDouble)

parseSchemeInteger :: Parser SchemeContent
parseSchemeInteger = integer >>= (return . SInteger)

parseSchemeSymbol :: Parser SchemeContent
parseSchemeSymbol = isymbol >>= (return . SSymbol)

parseSchemeBool :: Parser SchemeContent
parseSchemeBool =
  (true >>= (return . SBool)) <|>
  (false >>= (return . SBool))

parseSchemeVarible :: Parser SchemeContent
parseSchemeVarible = word >>= (return . SVarible)


parseSchemeValue :: Parser SchemeContent
parseSchemeValue =
  parseSchemeDouble <|>
  parseSchemeInteger <|>
  parseSchemeSymbol <|>
  parseSchemeBool <|>
  parseSchemeVarible

-- expression parser

parseSchemeAssignExpression :: Parser SchemeContent
parseSchemeAssignExpression = betweenbracket (
    liftA2 SAssignmentExpression
      (string "set!" *> spacesOrLineBreak *> word)
      (spacesOrLineBreak *> parseSchemeValue)
  )

-- define 有两种模式，定义常量或者是定义函数
parseSchemeDefineExpression ::Parser SchemeContent
parseSchemeDefineExpression = betweenbracket (
    liftA2 SDefinitionExpreaaion
      (string "define" *> spacesOrLineBreak *> word)
      (spacesOrLineBreak *> (
        parseSchemeValue <|>
        liftA2
          SLambdaExpression
          (betweenbracket (some (spacesOrLineBreak *> word)))
          parseScheme
      ))
  )

parseSchemeIfExpression :: Parser SchemeContent
parseSchemeIfExpression = betweenbracket (
    liftA3 SIfExpression
      (string "if" *> spacesOrLineBreak *> parseSchemeValue)
      (spacesOrLineBreak *> parseScheme)
      (spacesOrLineBreak *> parseScheme <* spacesOrLineBreak)
  )

-- 一般为 lambda 匿名函数表达式
parseSchemeLambdaExpression :: Parser SchemeContent
parseSchemeLambdaExpression = betweenbracket (
    liftA2
      SLambdaExpression
      (string "lambda" *> spacesOrLineBreak *> betweenbracket (some (spacesOrLineBreak *> word)))
      (spacesOrLineBreak *> parseScheme)
  )

parseSchemeBeginExpression :: Parser SchemeContent
parseSchemeBeginExpression = betweenbracket (
    fmap SBeginExpression
      (string "begin" *> some (spacesOrLineBreak *> parseScheme))
  )

parseSchemeApplicationExpression :: Parser SchemeContent
parseSchemeApplicationExpression = betweenbracket (
  liftA2 SApplicationExpression
    word
    (some (spacesOrLineBreak *> parseSchemeValue)))

parseSchemeExpression :: Parser SchemeContent
parseSchemeExpression =
  parseSchemeAssignExpression <|>
  parseSchemeDefineExpression <|>
  parseSchemeIfExpression <|>
  parseSchemeLambdaExpression <|>
  parseSchemeBeginExpression <|>
  parseSchemeApplicationExpression

parseScheme :: Parser SchemeContent
parseScheme = parseSchemeValue <|> parseSchemeExpression



