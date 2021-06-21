{-# LANGUAGE StandaloneDeriving #-}
-- instruction
module Instruction where

newtype Reg = Reg String deriving Show
newtype Op  = Op String deriving Show
newtype Label = Label String deriving (Show, Eq)

-- Map 中 key 对应的数据类型需要
deriving instance Ord Label

data InstValue = ISymbol String
               | IInteger Integer
               | IDouble Double
               | IString String
               | IList [InstValue]
               | INull
               | IBool Bool
               -- 上面为 scheme 中的值，以下为汇编语言的中的值
               | IRegLabel String
               | ILabel String
               deriving Show

instance Eq InstValue where
  -- double 与 integer 可以进行比较
  (==) (IInteger v) (IInteger v') = v == v'
  (==) (IInteger v) (IDouble v') = fromInteger v == v'
  (==) (IDouble v) (IInteger v') = v == fromInteger v'
  (==) (IDouble v) (IDouble v') = v == v'
  (==) (IString v) (IString v') = v == v'
  (==) (ISymbol v) (ISymbol v') = v == v'
  (==) INull INull = True
  (==) (IBool v) (IBool v') = v == v'
  (==) (IRegLabel v) (IRegLabel v') = v == v'
  (==) (ILabel v) (ILabel v') = v == v'
  (==) (IList v) (IList v') = any (uncurry (==)) (zip v v')
  (==) a b = error ("\"=\" expect two same type value, but actally " ++ show a ++ "," ++ show b)

data Inst = AssignRegReg Reg Reg
          | AssignRegConst Reg InstValue
          | AssignRegOpResult Reg Op [InstValue]
          | AssignRegLabel Reg Label
          | Perform Op [InstValue]
          | Test Op [InstValue]
          | Branch Label
          | GotoLabel Label
          | GotoReg Reg
          | SaveReg Reg
          | RestoreReg Reg
          deriving Show
