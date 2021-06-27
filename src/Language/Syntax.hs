module Language.Syntax where

type Varible = String

data SchemeContent = SDouble Double
                  | SInteger Integer
                  | SString String
                  | SSymbol String
                  | SBool Bool
                  | SVarible Varible
                  | SAssignmentExpression Varible SchemeContent
                  | SDefinitionExpreaaion Varible SchemeContent
                  | SIfExpression SchemeContent SchemeContent SchemeContent
                  | SLambdaExpression [Varible] SchemeContent
                  | SBeginExpression [SchemeContent]
                  | SApplicationExpression Varible [SchemeContent]
                  | SUnknownExpression
                  deriving Show





