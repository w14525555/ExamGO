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

-- test2 = TestCase $
--   assertEqual "Test failS returns Left Nothing"
--   ((testRunGlobal failS) :: Maybe ErrMsg)$
--   (Left (Just "test"))

-- test3 = TestCase $
--   assertEqual "Test failH returns Left Just errorMsg"
--   (testRunGlobal (failH "fail test")) $
--   Left $ Just "fail test"
  

globalTest = [test1]

-- Combine All the Tests
tests = test[globalTest]

main = runTestTT tests