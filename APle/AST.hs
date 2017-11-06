module AST where

type ErrMsg = String  -- human-readable error messages
type VName = String   -- variable names
type FName = String   -- function (including operator) names
type PName = String   -- predicate names

data Term =
    TVar VName
  | TNum Integer
  | TFun FName [Term]
  deriving (Eq, Ord, Show, Read)

data Cond = Cond PName [Term] [Term]
  deriving (Eq, Show, Read)

data Rule = Rule Term Term [Cond]
  deriving (Eq, Show, Read)

data Cmd =
    CRule Rule
  | CQuery Term Bool{-verbosity flag-}
  deriving (Eq, Show, Read)

data Fixity = FLeft | FRight | FNone
  deriving (Eq, Show, Read)

data OpTable = OpTable [(Fixity, [FName])]
  deriving (Eq, Show, Read)

-- the remaing definitions only relate to the Semantics part
data Rsp = Rsp [Term] (Maybe ErrMsg)
  deriving (Eq, Show)

maxSteps :: Int
maxSteps = 1000
