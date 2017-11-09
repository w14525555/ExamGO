import AST
import Syntax

import Test.QuickCheck

-- Your code here
opt :: OpTable
opt = OpTable [(FNone, ["<=", "<"]), (FLeft, ["+", "-"]), (FLeft, ["*"]), (FRight, ["**"])]

characters :: String
characters = ['a'..'z'] ++ ['A'..'Z']

numbers :: String
numbers = ['0'..'9']

-- A quick test to test parse number
-- if n < 0, add "~" before the absolute value of n
prop_int :: Integer -> Bool
prop_int n = if n >= 0
    then (parseStringTerm opt (show n)) == (Right (TNum n))
    else (parseStringTerm opt ("~" ++ (show (abs n)))) == (Right (TNum n))

-- A quick check test to test parse string
genSyntaxString :: Gen String
genSyntaxString = listOf1 $ elements $ characters ++ numbers

genChar :: Gen String
genChar = resize 1 $ listOf1 $ elements characters

newtype SyntaxString = SyntaxString { genString :: String }
    deriving Show

instance Arbitrary SyntaxString where
    arbitrary = do
        c <- genChar
        cs <- genSyntaxString
        return $ SyntaxString (c ++ cs)

prop_string :: SyntaxString -> Bool
prop_string s = let str = (genString s) in
    (parseStringTerm opt str) == (Right $ TVar str)

-- main test function, to make tests runnable as "runhaskell SyntaxQC"
main :: IO ()
main = quickCheck prop_int >> quickCheck prop_string
