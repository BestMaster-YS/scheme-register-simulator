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

selfEvaluating :: SchemeContent -> Bool
selfEvaluating (SDouble _) = True
selfEvaluating (SInteger _) = True
selfEvaluating (SString _) = True
selfEvaluating (SBool _) = True
selfEvaluating _ = False

isVarible :: SchemeContent -> Bool
isVarible (SVarible _) = True
isVarible _ = False

isSymbol :: SchemeContent -> Bool
isSymbol (SSymbol _) = True
isSymbol _ = False

isAssignment :: SchemeContent -> Bool
isAssignment (SAssignmentExpression _ _) = True
isAssignment _ = False

isDefinition :: SchemeContent -> Bool
isDefinition (SDefinitionExpreaaion _ _) = True
isDefinition _ = False

isIf :: SchemeContent -> Bool
isIf SIfExpression {} = True
isIf _ = False 

isLambda :: SchemeContent -> Bool
isLambda (SLambdaExpression _ _) = True
isLambda _ = False

isBegin :: SchemeContent -> Bool
isBegin (SBeginExpression _) = True
isBegin _ = False

isApplication :: SchemeContent -> Bool
isApplication (SApplicationExpression _ _) = True
isApplication _ = False


