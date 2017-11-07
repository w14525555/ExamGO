import Test.HUnit
import SemanticsImpl
import AST

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
  (Right ((),[("test",TNum 1)]))

test4 = TestCase $
  assertEqual "Test match with diffrent function name returns failS"
  (testRunLocal (matchTerm (TFun "ss" []) (TFun "s" [])))$
  (Left Nothing)


localTest = [test2, test3, test4]
-- Combine All the Tests
tests = test[globalTest ++ localTest]


main = runTestTT tests