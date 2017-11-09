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

-- A quick check test to test parse string
-- generate a random non-empty string start with char
genChar :: Gen String
genChar = resize 1 $ listOf1 $ elements characters

genSyntaxString :: Gen String
genSyntaxString = listOf1 $ elements $ characters ++ numbers

newtype SyntaxString = SyntaxString { genString :: String } deriving Show

instance Arbitrary SyntaxString where
    arbitrary = do
        c <- genChar
        cs <- genSyntaxString
        return $ SyntaxString (c ++ cs)

instance Arbitrary Term where
    arbitrary = oneof [genTermString, genTermNumbers, genTermFunction]

genNumbers :: Gen Integer
genNumbers = choose (-999999999999999999, 999999999999999999)

genTermNumbers :: Gen Term
genTermNumbers = do
    ns <- genNumbers
    return $ TNum ns

genTermString :: Gen Term
genTermString = do
    c <- genChar
    cs <- genSyntaxString
    return $ TVar $ c ++ cs

genTermFunction :: Gen Term
genTermFunction = do
    c <- genChar
    cs <- genSyntaxString
    ts <- oneof [genTermString, genTermNumbers, genTermFunction, genFunctionWithEmptyTermList]
    return $ TFun (c++cs) [ts]

genFunctionWithEmptyTermList :: Gen Term
genFunctionWithEmptyTermList = do
    c <- genChar
    cs <- genSyntaxString
    return $ TFun (c++cs) []

-- A quick test to test parse number
-- if n < 0, add "~" before the absolute value of n

prop_int :: Integer -> Bool
prop_int n = if n >= 0
    then (parseStringTerm opt (show n)) == (Right (TNum n))
    else (parseStringTerm opt ("~" ++ (show (abs n)))) == (Right (TNum n))

prop_string :: SyntaxString -> Bool
prop_string s = let str = (genString s) in
    (parseStringTerm opt str) == (Right $ TVar str)

convert_term_to_string :: Term -> String
convert_term_to_string (TVar name) = name
convert_term_to_string (TNum num) = if num >= 0 
    then show num
    else ("~" ++ (show (abs num)))
convert_term_to_string (TFun name []) = name ++ "(" ++ ")"
convert_term_to_string (TFun name ts) = name ++ "(" ++ (convert_terms_to_string ts) ++ ")"

convert_terms_to_string :: [Term] -> String
convert_terms_to_string [t] = convert_term_to_string t
convert_terms_to_string (t:ts) = (convert_terms_to_string [t]) ++ "," ++ (convert_terms_to_string ts) 

-- The generated term should be valid
-- So it should not go to left branch
prop_term :: Term -> Bool
prop_term t = case parseStringTerm opt (convert_term_to_string t) of
    Right result -> t == t
    Left _ -> False 

-- main test function, to make tests runnable as "runhaskell SyntaxQC"
main :: IO ()
main = quickCheck prop_int >> quickCheck prop_string >> quickCheck prop_term
