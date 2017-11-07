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
  return v = Global $ \_ -> Right v
  t >>= f = Global $ \r -> do result1 <- runGlobal t r
                              result2 <- runGlobal (f result1) r
                              return result2
  fail "Nothing" = Global $ \_ -> Left Nothing
  fail s = Global $ \_ -> Left $ Just s

-- You may modify these if you want, but it shouldn't be necessary
instance Functor Global where fmap = liftM
instance Applicative Global where pure = return; (<*>) = ap

initialRules :: [Rule]
initialRules = [ Rule (TFun "+" [TNum 0,TVar "t"]) (TVar "t") [],
                 Rule (TFun "+" [TVar "t",TNum 0]) (TVar "t") [],
                 Rule (TFun "+" [TVar "t1",TFun "+" [TVar "t2",TVar "t3"]])
                (TFun "+" [TFun "+" [TVar "t1",TVar "t2"],TVar "t3"]) [],
                 Rule (TFun "+" [TVar "t",TVar "t"]) (TFun "*" [TNum 2,TVar "t"]) [],
                 Rule (TFun "*" [TNum 0,TVar "t"]) (TNum 0) []]

getRules :: Global [Rule]
getRules = Global $ \r -> Right r

failS :: Global a
failS = fail "Nothing"

failH :: ErrMsg -> Global a
failH s = fail s

tryS :: Global a -> Global a -> Global a
tryS m1 m2 = case runGlobal m1 initialRules of
  Right result1 -> return result1
  Left s1 -> case s1 of
    Nothing -> case runGlobal m2 initialRules of
      Right result2 -> return result2
      Left s2 -> case s2 of
        Nothing -> fail "Nothing"
        Just v2 -> fail v2
    Just v1 -> fail v1

testRunGlobal :: Global a -> Either (Maybe ErrMsg) a
testRunGlobal m = case runGlobal m initialRules of
  Right v -> Right v
  Left s -> Left s 

---- Local monad and related functions

type LEnv = [(VName, Term)]

newtype Local a = Local {runLocal :: LEnv -> Global (a, LEnv)}

instance Monad Local where
  return v = Local $ \e -> Global $ \_ -> Right (v, e)
  t >>= f = Local $ \e ->
    do (result1, newState1) <- runLocal t e
       (result2, newState2) <- runLocal (f result1) newState1
       return (result2, newState2)
  fail "Nothing" = Local $ \_ -> Global $ \_ -> Left Nothing
  fail s = Local $ \_ -> Global $ \_ -> Left $ Just s

instance Functor Local where fmap = liftM
instance Applicative Local where pure = return; (<*>) = ap

inc :: Global a -> Local a
inc g = undefined

--  Global $ \_ -> Local $ \_ -> g

askVar :: VName -> Local Term
askVar name = Local $ \e -> case findTerm name e of
  [] -> failH "variable is unbound"
  [t] -> return (t, e)

tellVar :: VName -> Term -> Local ()
tellVar name t = Local $ \e -> case findTerm name e of
  [] -> return ((), e ++ [(name, t)])
  [t2] -> if t == t2 then return ((), e) else failS

-- A function to create a initial context for Local
initialLocalEnv :: LEnv
initialLocalEnv = [("test", TNum 1)]

-- A function to unwrap the value insides the monds
testRunLocal :: Local a -> Either (Maybe ErrMsg) (a, LEnv)
testRunLocal m = testRunGlobal (runLocal m initialLocalEnv)

-- A function to get the Value of a Variable
-- If unbounded, return []
-- Else return [t]
findTerm :: VName -> LEnv -> [Term]
findTerm vname [] = []
findTerm vname [(name, t)] = if name == vname then [t] else []
findTerm vname (n:ns) = (findTerm vname [n]) ++ (findTerm vname ns)

---- Matching and instantiation

-- To matchTerm 
-- We can find 
matchTerm :: Term -> Term -> Local ()
-- If they are same constants, they can match: t + 0 and p + 0
matchTerm (TNum n1) (TNum n2) = if n1 == n2 then return () else fail "Nothing"
matchTerm (TVar nameP) (TNum n2) = tellVar nameP (TNum n2)
matchTerm (TVar nameP) (TFun fname2 ts) = tellVar nameP (TFun fname2 ts)
--If it is a variable name then get the Term from env
--And assign the new value 
matchTerm (TVar nameP) (TVar nameT) = do 
  t <- askVar nameT
  tellVar nameP t
matchTerm (TFun fname1 [])(TFun fname2 []) = 
  if fname1 == fname2 then return () else fail "Nothing"
matchTerm (TFun fname1 ts1)(TFun fname2 ts2) =
  if fname1 == fname2
    then do result <- matchTerms ts1 ts2
            case result of
              [] -> return ()
              _ -> fail "Nothing"
    else fail "Nothing"
matchTerm _ _ = fail "Nothing"

-- A function to handle the case of recusive terms
-- If two term list has different length
-- they don't not match
matchTerms :: [Term] -> [Term] -> Local [Term]
matchTerms [t1] [t2] = do matchTerm t1 t2; return []
matchTerms (t1:ts1) (t2:ts2) = do
  t <- matchTerms [t1] [t2]
  ts <- matchTerms ts1 ts2
  return $ t ++ ts
matchTerms _ _ = fail "Nothing"

instTerm :: Term -> Local Term
instTerm (TVar vname) = askVar vname
instTerm (TNum n) = return $ TNum n
instTerm (TFun fname []) = return (TFun fname [])
instTerm (TFun fname ts) = do
  newTs <- instTerms ts
  return $ TFun fname newTs

instTerms :: [Term] -> Local [Term]
instTerms [t] = do
  t1 <- instTerm t
  return [t1]
instTerms (t:ts) = do
  t1 <- instTerms [t] 
  ts1 <- instTerms ts
  return $ t1 ++ ts1

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
