import Test.HUnit
import SemanticsImpl
import AST

-- A function to unwrap the value insides local monds
testRunLocal :: Local a -> Either (Maybe ErrMsg) (a, LEnv)
testRunLocal m = testRunGlobal (runLocal m initialLocalEnv)

-- A function used unwrpped the value inside global monads
testRunGlobal :: Global a -> Either (Maybe ErrMsg) a
testRunGlobal m = case runGlobal m initialRules of
  Right v -> Right v
  Left s -> Left s

-- Tests for global
test1 = TestCase $
  assertEqual "Test Get Rules returns the right rules"
  (testRunGlobal getRules) $
  Right [Rule (TFun "+" [TNum 0,TVar "t"]) (TVar "t") [],
  Rule (TFun "+" [TVar "t",TNum 0]) (TVar "t") [],
  Rule (TFun "+" [TVar "t1",TFun "+" [TVar "t2",TVar "t3"]]) 
  (TFun "+" [TFun "+" [TVar "t1",TVar "t2"],TVar "t3"]) [],
  Rule (TFun "+" [TVar "t",TVar "t"]) (TFun "*" [TNum 2,TVar "t"]) [],
  Rule (TFun "*" [TNum 0,TVar "t"]) (TNum 0) []]

globalTest = [test1]

-- Local Tests

-- Match Tests
test2 = TestCase $
  assertEqual "Test match with different type returns failS"
  (testRunLocal (matchTerm (TNum 1) (TVar "name")))$
  (Left Nothing)

test3 = TestCase $
  assertEqual "Test match with number type returns empty"
  (testRunLocal (matchTerm (TNum 1) (TNum 2)))$
  (Left Nothing)

test4 = TestCase $
  assertEqual "Test match with diffrent function name returns failS"
  (testRunLocal (matchTerm (TFun "ss" []) (TFun "s" [])))$
  (Left Nothing)

test5 = TestCase $
  assertEqual "Test match when P is an avariable and T is a Num will assign p with value of T"
  (testRunLocal (matchTerm (TVar "name") (TNum 3)))$
  (Right ((),[("test",TNum 1),("name",TNum 3)]))

test6 = TestCase $
  assertEqual "Test match when P is an avariable and T is a Function will assign p with value of T"
  (testRunLocal (matchTerm (TVar "name") (TFun "plus" [(TNum 1), (TNum 2)])))$
  (Right ((),[("test",TNum 1),("name",TFun "plus" [TNum 1,TNum 2])]))

test7 = TestCase $
  assertEqual "Test match two function t + 0 and 1 + 0"
  (testRunLocal (matchTerm (TFun "plus" [(TVar "p"), (TNum 0)]) (TFun "plus" [(TNum 1), (TNum 0)])))$
  (Right ((),[("test",TNum 1),("p",TNum 1)]))

test8 = TestCase $
  assertEqual "Test match two function t + 0 and (1 + 1) + 0"
  (testRunLocal (matchTerm (TFun "plus" [(TVar "p"), (TNum 0)]) (TFun "plus" [(TFun "plus" [(TNum 1), (TNum 1)]), (TNum 0)])))$
  (Right ((),[("test",TNum 1),("p",TFun "plus" [TNum 1,TNum 1])]))

test9 = TestCase $
  assertEqual "Test match two function t + 0 and (1 + p) + 0 where p is unbound"
  (testRunLocal (matchTerm (TFun "plus" [(TVar "p"), (TNum 0)]) (TFun "plus" [(TFun "plus" [(TVar "v"), (TNum 1)]), (TNum 0)])))$
  (Left (Just "variable v is unbound"))

test10 = TestCase $
  assertEqual "Test match two function t + 0 and (1 + p) + 0 where p is bounded"
  (testRunLocal (matchTerm (TFun "plus" [(TVar "p"), (TNum 0)]) (TFun "plus" [(TFun "plus" [(TVar "test"), (TNum 1)]), (TNum 0)])))$
  (Right ((),[("test",TNum 1),("p",TFun "plus" [TNum 1,TNum 1])]))

matchTest = [test2, test3, test4, test5, test6, test7, 
             test8, test9, test10]

-- Eval Condition Tests
-- num
test11 = TestCase $
  assertEqual "Test evalCond num with Num Term returns right value"
  (testRunGlobal (evalCond "num" [(TNum 1)]))$
  (Right [TNum 1])

test12 = TestCase $
  assertEqual "Test evalCond num with a Var returns soft failure"
  (testRunGlobal (evalCond "num" [(TVar "name")]))$
  (Left (Just "evalCond with illegal Arguments"))

-- var
test13 = TestCase $
  assertEqual "Test evalCond var with a var Term returns right value"
  (testRunGlobal (evalCond "var" [(TVar "name")]))$
  (Right [TVar "name"])

test14 = TestCase $
  assertEqual "Test evalCond var with a Num returns soft failure"
  (testRunGlobal (evalCond "var" [(TNum 1)]))$
  (Left (Just "evalCond with illegal Arguments"))

-- add
test15 = TestCase $
  assertEqual "Test evalCond add success if n1 + n2 = n3"
  (testRunGlobal (evalCond "add" [(TNum 1), (TNum 2), (TNum 3)]))$
  (Right [TNum 1,TNum 2,TNum 3])

test16 = TestCase $
  assertEqual "Test evalCond add returns the sum of n1 + n2"
  (testRunGlobal (evalCond "add" [(TNum 5), (TNum 3), (TVar "name")]))$
  (Right [TNum 5,TNum 3,TNum 8])

test17 = TestCase $
  assertEqual "Test evalCond add returns soft fails if n1 + n2 /= n3"
  (testRunGlobal (evalCond "add" [(TNum 5), (TNum 3), (TNum 3)]))$
  (Left Nothing)

test18 = TestCase $
  assertEqual "Test evalCond add returns soft fails if type is not valid"
  (testRunGlobal (evalCond "add" [(TNum 5), (TVar "name"), (TNum 3)]))$
  (Left (Just "evalCond with illegal Arguments"))

-- mul
test19 = TestCase $
  assertEqual "Test evalCond mul returns the product of n1 * n2"
  (testRunGlobal (evalCond "mul" [(TNum 5), (TNum 3), (TVar "name")]))$
  (Right [TNum 5,TNum 3,TNum 15])

test20 = TestCase $
  assertEqual "Test evalCond mul returns soft fails if n1 * n2 /= n3"
  (testRunGlobal (evalCond "mul" [(TNum 5), (TNum 3), (TNum 3)]))$
  (Left Nothing)

test21 = TestCase $
  assertEqual "Test evalCond mul returns soft fails if type is not valid"
  (testRunGlobal (evalCond "mul" [(TNum 5), (TVar "name"), (TNum 3)]))$
  (Left (Just "evalCond with illegal Arguments"))

-- lexless
test22 = TestCase $
  assertEqual "Test evalCond lexless returns [Term] on Num n1 < Num n2"
  (testRunGlobal (evalCond "lexless" [(TNum 3), (TNum 5)]))$
  (Right [TNum 3,TNum 5])

test23 = TestCase $
  assertEqual "Test evalCond lexless returns [Term] on Var a < Var b"
  (testRunGlobal (evalCond "lexless" [(TVar "apple"), (TVar "name")]))$
  (Right [(TVar "apple"), (TVar "name")])

test24 = TestCase $
  assertEqual "Test evalCond lexless returns soft error on other cases"
  (testRunGlobal (evalCond "lexless" [(TNum 5), (TVar "name")]))$
  (Left (Just "evalCond with illegal Arguments"))

-- Undeinded Predicte
test25 = TestCase $
  assertEqual "Test evalCond return hardware error if the predicate is not defined"
  (testRunGlobal (evalCond "hi" [(TNum 5), (TVar "name")]))$
  (Left (Just "The predict hi is not defined"))

evalCondTest = [test11, test12, test13, test14, test15, test16, test17, test18
                ,test19, test20, test21, test22, test23, test24, test25]

-- Tests for apply Rule
-- t + 0 = t
plusZeroRule :: Rule
plusZeroRule = Rule (TFun "+" [TVar "t",TNum 0]) (TVar "t") []

-- t + t = 2 * t
doubleRule :: Rule
doubleRule = Rule (TFun "+" [TVar "t",TVar "t"]) (TFun "*" [TNum 2,TVar "t"]) []

-- -- A function to create a initial context for Local
-- initialLocalEnv :: LEnv
-- initialLocalEnv = [("test", TNum 1)]

test26 = TestCase $
  assertEqual "Test apply rule (t + 0 = t) work on a simple rule with 3 + 0"
  (testRunGlobal (applyRule plusZeroRule (TFun "+" [TNum 3,TNum 0])))$
  (Right (TNum 3))

test27 = TestCase $
  assertEqual "Test apply rule (t + 0 = t) work on a simple rule with v + 0"
  (testRunGlobal (applyRule plusZeroRule (TFun "+" [TVar "test",TNum 0])))$
  (Right (TNum 1))

test28 = TestCase $
  assertEqual "Test apply rule (t + t = 2 * t) work on a simple rule with 2 + 2"
  (testRunGlobal (applyRule doubleRule (TFun "+" [TNum 2,TNum 2])))$
  (Right (TFun "*" [TNum 2,TNum 2]))  

test29 = TestCase $
  assertEqual "Test apply rule (t + t = 2 * t) work on a simple rule with v + v (v = 1)"
  (testRunGlobal (applyRule doubleRule (TFun "+" [TNum 1,TNum 1])))$
  (Right (TFun "*" [TNum 2,TNum 1]))

-- Test apply rules with condition

-- t + 0 = t | num(t)
plusZeroRuleWithCond :: Rule
plusZeroRuleWithCond = Rule (TFun "+" [TVar "t",TNum 0]) (TVar "t") [Cond "num" [TVar "t"][]]

sumRuleWithCond :: Rule
sumRuleWithCond = Rule (TFun "+" [TVar "n1",TVar "n2"]) (TVar "n3") [Cond "num" [TVar "n1"] [],
                        Cond "num" [TVar "n2"] [],Cond "add" [TVar "n1",TVar "n2"] [TVar "n3"]]
sumRule :: Rule
sumRule = Rule (TFun "+" [TVar "n1",TVar "n2"]) (TVar "n3") []
 
test30 = TestCase $
  assertEqual "Test apply rule (t + 0 = t)| num(t) with condition num 3 with 3 + 0"
  (testRunGlobal (applyRule plusZeroRuleWithCond (TFun "+" [TNum 3,TNum 0])))$
  (Right (TNum 3))

test31 = TestCase $
  assertEqual "Test apply rule (t + 0 = t) num(t) work on a simple rule with v + 0"
  (testRunGlobal (applyRule plusZeroRuleWithCond (TFun "+" [TVar "test",TNum 0])))$
  (Right (TNum 1))

test32 = TestCase $
  assertEqual "Test apply rule (n1 + n2 = n3) | num(n1), num(n2), add(n1,n2;n3) work"
  (testRunGlobal (applyRule sumRuleWithCond (TFun "+" [TNum 2,TNum 3])))$
  (Right (TNum 5))

applyRuleTest = [test26, test27, test28, test29, test30, test31, test32]

-- Combine All the Tests
tests = test[globalTest ++ matchTest ++ evalCondTest ++ applyRuleTest]

main = runTestTT tests