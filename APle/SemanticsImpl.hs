module SemanticsImpl where

import AST
import Control.Monad

-- Put your implementation of the rewrite engine here. Feel free to
-- define your own auxilary functions as needed, but do not modify
-- the type definitions and declarations of the required individual
-- functions from what's specified here and in the assignment text.

---- Global monad and related functions

type GEnv = [Rule]

newtype Global a = Global {runGlobal :: GEnv -> Either (Maybe ErrMsg) a}

instance Monad Global where
  return v = undefined
  t >>= f = undefined

-- You may modify these if you want, but it shouldn't be necessary
instance Functor Global where fmap = liftM
instance Applicative Global where pure = return; (<*>) = ap
  
getRules :: Global [Rule]
getRules = undefined

failS :: Global a
failS = undefined

failH :: ErrMsg -> Global a
failH = undefined

tryS :: Global a -> Global a -> Global a
tryS = undefined

---- Local monad and related functions

type LEnv = [(VName, Term)]

newtype Local a = Local {runLocal :: LEnv -> Global (a, LEnv)}

instance Monad Local where
  return v = undefined
  t >>= f = undefined

instance Functor Local where fmap = liftM
instance Applicative Local where pure = return; (<*>) = ap

inc :: Global a -> Local a
inc = undefined

askVar :: VName -> Local Term
askVar = undefined

tellVar :: VName -> Term -> Local ()
tellVar = undefined

---- Matching and instantiation

matchTerm :: Term -> Term -> Local ()
matchTerm = undefined

instTerm :: Term -> Local Term
instTerm = undefined

---- Conditions and rule aplication

evalCond :: PName -> [Term] -> Global [Term]
evalCond = undefined

applyRule :: Rule -> Term -> Global Term
applyRule = undefined

---- Single-step term rewriting

rewriteTerm :: Term -> Global Term
rewriteTerm = undefined

---- Top-level interaction

processCmd :: Cmd -> GEnv -> (Rsp, GEnv)
processCmd = undefined
