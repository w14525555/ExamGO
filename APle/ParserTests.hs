module ParserTests where

import ParserImpl
import AST
import Test.HUnit

-- Unit Tests

-- Utility
opt :: OpTable
opt = OpTable [(FNone, ["<=", "<"]), (FLeft, ["+", "-"]), (FLeft, ["*"]), (FRight, ["**"])]

--Tests for Numbers
test1 = TestCase $
  assertEqual "Test a simple number"
  (parseStringTerm opt "123567890") $
  Right $ TNum 123567890

test2 = TestCase $
  assertEqual "Test a negative number"
  (parseStringTerm opt "~123567890") $
  Right $ TNum (-123567890)

test3 = TestCase $
  assertEqual "Test a number with whitespaces"
  (parseStringTerm opt "  ~123567890  ") $
  Right $ TNum (-123567890)
  
numberTest = [test1, test2, test3]

--Tests for vname, fname, pname 
test4 = TestCase $
  assertEqual "Test a simple name"
  (parseStringTerm opt "name") $
  Right $ TVar "name"

test5 = TestCase $
  assertEqual "Test a name including numbers"
  (parseStringTerm opt "name1244seggd") $
  Right $ TVar "name1244seggd"

test6 = TestCase $
  assertEqual "Test a name cannot start with a number"
  (parseStringTerm opt "1name") $
  Left "empty result probably with wrong intput"

vfpNameTest = [test4, test5, test6]

-- Tests for Term oper Term
test7 = TestCase $
  assertEqual "Test a simple operations for all"
  (parseStringTerm opt "1 + 2 * 3 - 5 ** 3") $
  Right $ TFun "-" [TFun "+" [TNum 1,TFun "*" [TNum 2,TNum 3]],TFun "**" [TNum 5,TNum 3]]

test8 = TestCase $
  assertEqual "Test a operation for less"
  (parseStringTerm opt "1 + 3 < 3") $
  Right $ TFun "<" [TFun "+" [TNum 1,TNum 3],TNum 3]

test9 = TestCase $
  assertEqual "Test all right associaty operations for **"
  (parseStringTerm opt "1 ** 3 ** 4") $
  Right $ TFun "**" [TNum 1,TFun "**" [TNum 3,TNum 4]]

test10 = TestCase $
  assertEqual "Test left associaty operations for + -"
  (parseStringTerm opt "1 + 3 - 4") $
  Right $ TFun "-" [TFun "+" [TNum 1,TNum 3],TNum 4]

test11 = TestCase $
  assertEqual "Test left associaty operations for *"
  (parseStringTerm opt "1 * 3 * 4") $
  Right $ TFun "*" [TFun "*" [TNum 1,TNum 3],TNum 4]

test12 = TestCase $
  assertEqual "Test precedence for all operations *"
  (parseStringTerm opt "1 + 3 * 4 ** 5 < 2") $
  Right $ TFun "<" [TFun "+" [TNum 1,TFun "*" [TNum 3,TFun "**" [TNum 4,TNum 5]]],TNum 2] 

operatorTest = [test7, test8, test9, test10, test11, test12]

-- Tests for fname '(' Termz ')' 
test13 = TestCase $
  assertEqual "Test for the case when Term is empty"
  (parseStringTerm opt "name ( ) ") $
  Right $ TFun "name" []

test14 = TestCase $
  assertEqual "Test fname term with single term"
  (parseStringTerm opt "name (1 + 3 ) ") $
  Right $ TFun "name" [TFun "+" [TNum 1,TNum 3]]

test15 = TestCase $
  assertEqual "Test fname term with multiple terms"
  (parseStringTerm opt "name (1 + 3, hello, 3*5 ) ") $
  Right (TFun "name" [TFun "+" [TNum 1,TNum 3],TVar "hello",TFun "*" [TNum 3,TNum 5]])

functionTermTest = [test13, test14, test15]

-- Test for ( Term )
parentheseTermTest = TestCase $
  assertEqual "Test for a simple parenthese term"
  (parseStringTerm opt "(3 <= 2) ") $
  Right $ TFun "<=" [TNum 3,TNum 2] 

-- Test for the given example give in Exam txt
givenExampleTest = TestCase $
  assertEqual "Test the given example in the exam text"
  (parseStringTerm opt "3*(x+f(y,4)+z)") $
  Right (TFun "*" [TNum 3,TFun "+" [TFun "+" [TVar "x",TFun "f" [TVar "y",TNum 4]],TVar "z"]])

-- Tests for Cond
test18 = TestCase $
  assertEqual "Test Cond with only one term"
  (parseStringCmds opt " 1 = 5 | name ( 1 + 3 ).") $
  Right [CRule (Rule (TNum 1) (TNum 5) [Cond "name" [TFun "+" [TNum 1,TNum 3]] []])]

test19 = TestCase $
  assertEqual "Test Cond with several terms"
  (parseStringCmds opt "1 = 5 | name ( 1 + 3 ; f(y, 3) ).") $
  Right [CRule (Rule (TNum 1) (TNum 5) [Cond "name" [TFun "+" [TNum 1,TNum 3]] [TFun "f" [TVar "y",TNum 3]]])]

condTest = [test18, test19]

-- Test for Rules
test20 = TestCase $
  assertEqual "Test a rule with empty conds"
  (parseStringCmds opt "1 + 3 = 3 * 5.") $
  Right [CRule (Rule (TFun "+" [TNum 1,TNum 3]) (TFun "*" [TNum 3,TNum 5]) [])]  

test21 = TestCase $
  assertEqual "Test a rule with empty conds"
  (parseStringCmds opt "1 + 3 = 3 * 5 | f(3), f(z, 5).") $
  Right [CRule (Rule (TFun "+" [TNum 1,TNum 3]) (TFun "*" [TNum 3,TNum 5]) [Cond "f" [TNum 3] [],Cond "f" [TVar "z",TNum 5] []])] 
rulTest = [test20, test21]

-- Test for Cmd
test22 = TestCase $
  assertEqual "Test a empty cmd"
  (parseStringCmds opt " ") $
  Right []

test23 = TestCase $
  assertEqual "Test a rule cmd"
  (parseStringCmds opt " 3 = 5 .") $
  Right [CRule (Rule (TNum 3) (TNum 5) [])]

test24 = TestCase $
  assertEqual "Test a cmd with one question mark"
  (parseStringCmds opt " 3 + 5 ?") $
  Right [CQuery (TFun "+" [TNum 3,TNum 5]) False]

test25 = TestCase $
  assertEqual "Test a cmd with two question mark"
  (parseStringCmds opt " 3 + 5 ??") $
  Right [CQuery (TFun "+" [TNum 3,TNum 5]) True]

test26 = TestCase $
  assertEqual "Test a cmd cannot have more than two question mark"
  (parseStringCmds opt " 3 + 5 ???") $
  Left "empty result probably with wrong intput"

test27 = TestCase $
  assertEqual "Test multiple Cmd"
  (parseStringCmds opt " 3 + 5 ?? 2 < 3 ?") $
  Right [CQuery (TFun "+" [TNum 3,TNum 5]) True,CQuery (TFun "<" [TNum 2,TNum 3]) False]

test28 =  TestCase $
  assertEqual "Test multiple Cmd"
  (parseStringCmds opt "D(x,t1*t2) = t1*D(x,t2) + t2*D(x,t1).") $
  Right [CRule (Rule (TFun "D" [TVar "x",TFun "*" [TVar "t1",TVar "t2"]]) 
  (TFun "+" [TFun "*" [TVar "t1",TFun "D" [TVar "x",TVar "t2"]],
  TFun "*" [TVar "t2",TFun "D" [TVar "x",TVar "t1"]]]) [])]
  

cmdTest = [test22, test23, test24, test25, test26, test27, test28]

-- Test for the Optation

tests = test[numberTest ++ vfpNameTest ++ operatorTest ++ functionTermTest
            ++ [parentheseTermTest, givenExampleTest] ++ condTest ++ rulTest
            ++ cmdTest]

main = runTestTT tests
