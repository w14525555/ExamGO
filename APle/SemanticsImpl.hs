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

getRules :: Global [Rule]
getRules = Global $ \r -> Right r

-- A function to create a initial context for Global
initialRules :: [Rule]
initialRules = [ Rule (TFun "+" [TNum 0,TVar "t"]) (TVar "t") [],
                 Rule (TFun "+" [TVar "t",TNum 0]) (TVar "t") [],
                 Rule (TFun "+" [TVar "t1",TFun "+" [TVar "t2",TVar "t3"]])
                (TFun "+" [TFun "+" [TVar "t1",TVar "t2"],TVar "t3"]) [],
                 Rule (TFun "+" [TVar "t",TVar "t"]) (TFun "*" [TNum 2,TVar "t"]) [],
                 Rule (TFun "*" [TNum 0,TVar "t"]) (TNum 0) []]

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

---- Local monad and related functions

type LEnv = [(VName, Term)]

newtype Local a = Local {runLocal :: LEnv -> Global (a, LEnv)}

instance Monad Local where
  return v = Local $ \e -> Global $ \_ -> Right (v, e)
  t >>= f = Local $ \e ->
    do (result1, newState1) <- runLocal t e
       (result2, newState2) <- runLocal (f result1) newState1
       return (result2, newState2)

instance Functor Local where fmap = liftM
instance Applicative Local where pure = return; (<*>) = ap

-- A function to create a initial context for Local
initialLocalEnv :: LEnv
initialLocalEnv = [("test", TNum 1)]

-- Convert global monads
inc :: Global a -> Local a
inc g = Local $ \e -> Global $ \r -> case runGlobal g r of
  Right v -> Right (v, e)
  Left s -> Left s

askVar :: VName -> Local Term
askVar name = Local $ \e -> case findTerm name e of
  [] -> failH $ "variable " ++ name ++" is unbound"
  [t] -> return (t, e)

tellVar :: VName -> Term -> Local ()
tellVar name t = Local $ \e -> case findTerm name e of
  [] -> return ((), e ++ [(name, t)])
  [t2] -> if t == t2 then return ((), e) else failS

-- A function to get the Value of a Variable
-- If unbounded, return []
-- Else return [t]
findTerm :: VName -> LEnv -> [Term]
findTerm vname [] = []
findTerm vname [(name, t)] = if name == vname then [t] else []
findTerm vname (n:ns) = (findTerm vname [n]) ++ (findTerm vname ns)

---- Matching and instantiation

-- To matchTerm 
matchTerm :: Term -> Term -> Local ()
-- If they are same constants, they can match: 1 + 0 and 1 + 0
matchTerm (TNum n1) (TNum n2) = if n1 == n2 then return () else inc failS
-- if p is an avariable p + 0 and 3 + 0 
-- And assign value 3 to p to finish the match
matchTerm (TVar nameP) (TNum n2) = tellVar nameP (TNum n2)
-- if p is an avariable, it can aslo match as long as
-- we can assign the function to p  
matchTerm (TVar nameP) (TFun fname2 ts) = do
  t <- instTerm $ TFun fname2 ts
  tellVar nameP t
--If it is a variable name then get the Term from env
--And assign the new value 
matchTerm (TVar nameP) (TVar nameT) = do
  t <- askVar nameT
  tellVar nameP t
matchTerm (TFun fname1 [])(TFun fname2 []) = 
  if fname1 == fname2 then return () else inc failS
matchTerm (TFun fname1 ts1)(TFun fname2 ts2) =
  if fname1 == fname2
    then do result <- matchTerms ts1 ts2
            case result of
              [] -> return ()
              _ -> inc failS
    else inc failS
matchTerm _ _ = inc failS

-- A function to handle the case of recusive terms
-- If two term list has different length
-- they don't match
matchTerms :: [Term] -> [Term] -> Local [Term]
matchTerms [t1] [t2] = do matchTerm t1 t2; return []
matchTerms (t1:ts1) (t2:ts2) = do
  t <- matchTerms [t1] [t2]
  ts <- matchTerms ts1 ts2
  return $ t ++ ts
matchTerms _ _ = inc failS

instTerm :: Term -> Local Term
instTerm (TVar vname) = askVar vname
instTerm (TNum n) = return $ TNum n
instTerm (TFun fname []) = return (TFun fname [])
instTerm (TFun fname ts) = do
  newTs <- instTerms ts
  return $ TFun fname newTs

instTerms :: [Term] -> Local [Term]
instTerms [] = return []
instTerms [t] = do
  t1 <- instTerm t
  return [t1]
instTerms (t:ts) = do
  t1 <- instTerms [t] 
  ts1 <- instTerms ts
  return $ t1 ++ ts1

illegalArg :: String
illegalArg = "evalCond with illegal Arguments"
-- Conditions and rule aplication
-- tellVar call failS if t3 is bounded but 
-- binding to the value n1 + n2
evalCond :: PName -> [Term] -> Global [Term]
evalCond "num" [(TNum n)] = return [(TNum n)]
evalCond "num" _ = failH illegalArg
evalCond "var" [(TVar v)] = return [(TVar v)]
evalCond "var" _ = failH illegalArg
evalCond "add" [(TNum n1), (TNum n2), (TNum n3)] = 
  if n1 + n2 == n3 then return [(TNum n1), (TNum n2), (TNum n3)] else failS
evalCond "add" [(TNum n1), (TNum n2), (TVar name)] = return [(TNum n1), (TNum n2), (TNum (n1 + n2))]
evalCond "add" _ = failH illegalArg
evalCond "mul" [(TNum n1), (TNum n2), (TNum n3)] = 
  if n1 * n2 == n3 then return [(TNum n1), (TNum n2), (TNum n3)] else failS
evalCond "mul" [(TNum n1), (TNum n2), (TVar name)] = return [(TNum n1), (TNum n2), (TNum (n1 * n2))]
evalCond "mul" _ = failH illegalArg
evalCond "lexless" [(TNum n1), (TNum n2)] =
  if n1 < n2 then return [(TNum n1), (TNum n2)] else failS
evalCond "lexless" [(TVar v1), (TVar v2)] = 
  if v1 < v2 then return [(TVar v1), (TVar v2)] else failS
evalCond "lexless" _ = failH illegalArg
evalCond p _ = failH $ "The predict " ++ p  ++ " is not defined"

-- Use runlocal to convert local to global
-- we can get a new LEnv from runLocal matchTerm
-- use it to instTerm
applyRule :: Rule -> Term -> Global Term
applyRule (Rule t1 t2 []) t3 = do
  (_, e) <- runLocal (matchTerm t1 t3) initialLocalEnv
  (t, _) <- runLocal (instTerm t2) e
  return t
applyRule (Rule t1 t2 conds) t3 = do
  (_, e) <- runLocal (matchTerm t1 t3) initialLocalEnv
  newTs <- checkConds conds e
  (_, newE) <- runLocal (assignConds conds newTs) e
  (t, _) <- runLocal (instTerm t2) newE
  return t

-- A function used to check conditions
-- It will firstly init the Terms in the
-- The conditional rule then do the evaluation
checkConds :: [Cond] -> LEnv -> Global [Term]
checkConds [(Cond pName ts1 ts2)] e = do
  (newTs1, _) <- runLocal (instTerms ts1) e
  evalCond pName (newTs1 ++ ts2)
checkConds (c:cs) e = do
  r <- checkConds [c] e
  rs <- checkConds cs e
  return $ r ++ rs

-- A function to assign the evaluate result to the RHS
-- For instance 3 + 4 = y
-- When we evalCon we get 3 + 4 = 7
-- Then we match the list 
-- THe match function will assign y with 7
assignConds :: [Cond] -> [Term] -> Local [Term]
assignConds conds ts = matchTerms (getTermsFromConds conds) ts

-- A function to get all the [Item] in the condition list
getTermsFromConds :: [Cond] -> [Term]
getTermsFromConds [(Cond pName ts1 ts2)] = ts1 ++ ts2
getTermsFromConds (c:cs) = (getTermsFromConds [c]) ++ (getTermsFromConds cs)

---- Single-step term rewriting
rewriteTerm :: Term -> Global Term
rewriteTerm t = undefined

-- findRuleForTerm :: Term -> Rule
-- findRuleForTerm = undefined

---- Top-level interaction

processCmd :: Cmd -> GEnv -> (Rsp, GEnv)
processCmd = undefined

-- A function used unwrpped the value inside global monads
-- testRunGlobal :: Global a -> Either (Maybe ErrMsg) a
-- testRunGlobal m = case runGlobal m initialRules of
--   Right v -> Right v
--   Left s -> Left s

-- -- A function to create a initial context for Local
-- initialLocalEnv :: LEnv
-- initialLocalEnv = [("test", TNum 1)]

-- A function to unwrap the value insides the monds
-- testRunLocal :: Local a -> Either (Maybe ErrMsg) (a, LEnv)
-- testRunLocal m = testRunGlobal (runLocal m initialLocalEnv)
