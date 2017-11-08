import Test.HUnit
import SemanticsImpl
import AST

-- A function to create a initial context for Local
initialLocalEnv :: LEnv
initialLocalEnv = [("test", TNum 1)]

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

-- test3 = TestCase $
--   assertEqual "Test failH returns Left Just errorMsg"
--   (testRunGlobal (failH "fail test")) $
--   Left $ Just "fail test"

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

localTest = [test2, test3, test4]

-- Eval Condition Tests
test5 = TestCase $
  assertEqual "Test evalCond num with Num Term returns right value"
  (testRunGlobal (evalCond "num" [(TNum 1)]))$
  (Right [TNum 1])

test6 = TestCase $
  assertEqual "Test evalCond num with a Var returns soft failure"
  (testRunGlobal (evalCond "num" [(TVar "name")]))$
  (Left Nothing)

test7 = TestCase $
  assertEqual "Test evalCond var with a var Term returns right value"
  (testRunGlobal (evalCond "var" [(TVar "name")]))$
  (Right [TVar "name"])

test8 = TestCase $
  assertEqual "Test evalCond var with a Num returns soft failure"
  (testRunGlobal (evalCond "var" [(TNum 1)]))$
  (Left Nothing)

test9 = TestCase $
  assertEqual "Test evalCond add success if n1 + n2 = n3"
  (testRunGlobal (evalCond "add" [(TNum 1), (TNum 2), (TNum 3)]))$
  (Right [TNum 1,TNum 2,TNum 3])

test10 = TestCase $
  assertEqual "Test evalCond add returns the sum of n1 + n2"
  (testRunGlobal (evalCond "add" [(TNum 5), (TNum 3), (TVar "name")]))$
  (Right [TNum 5,TNum 3,TNum 8])

test11 = TestCase $
  assertEqual "Test evalCond add returns soft fails if n1 + n2 /= n3"
  (testRunGlobal (evalCond "add" [(TNum 5), (TNum 3), (TNum 3)]))$
  (Left Nothing)

evalCondTest = [test5, test6, test7, test8, test9, test10, test11]

-- Combine All the Tests
tests = test[globalTest ++ localTest ++ evalCondTest]


main = runTestTT tests