module PrinterImpl where

import AST
import Data.Maybe(fromJust)
import Data.List

-- do not change the type!
printTerm :: OpTable -> Term -> String
printTerm opt (TVar v) = v
printTerm opt (TNum i) = show i
printTerm opt (TFun fName ts) = if fName `elem` (getNameList opt)
    then (checkParenthese fName ts opt)
    else fName ++ "(" ++ (printNormalFuncionTerms ts opt) ++ ")"

--fName ++ "(" ++ (printTerms ts opt) ++ ")" 

getNameList :: OpTable -> [FName]
getNameList (OpTable [(_, names)]) = names
getNameList (OpTable (n:ns)) = (getNameList (OpTable [n])) ++ (getNameList (OpTable ns)) 

printOptFuncionTail :: [Term] -> OpTable -> String
printOptFuncionTail [t] opt = (printTerm opt t) ++ ")" 
printOptFuncionTail (t:ts) opt = (printOptFuncionTail [t] opt) ++ (printOptFuncionTail ts opt)

printOptFuncionHead :: FName -> [Term] -> OpTable -> String
printOptFuncionHead fName [t] opt = (printTerm opt t) ++ fName ++ "("
printOptFuncionHead fName (t:ts) opt = (printOptFuncionHead fName [t] opt) ++ (printOptFuncionTail ts opt)

printOptFuncionHeadWithoutParen :: FName -> [Term] -> OpTable -> String
printOptFuncionHeadWithoutParen fName [t] opt = (printTerm opt t) ++ fName
printOptFuncionHeadWithoutParen fName (t:ts) opt = (printOptFuncionHeadWithoutParen fName [t] opt) ++ (printNormalFuncionTerms ts opt)

printNormalFuncionTerms :: [Term] -> OpTable -> String
printNormalFuncionTerms [t] opt = printTerm opt t 
printNormalFuncionTerms (t:ts) opt = (printNormalFuncionTerms [t] opt) ++ "," ++ (printNormalFuncionTerms ts opt) 

printOptFuncionTerm :: FName -> [Term] -> OpTable -> String
printOptFuncionTerm fName [t] opt = printTerm opt t
printOptFuncionTerm fName (t:ts) opt = printOptFuncionHead fName (t:ts) opt

checkParenthese :: FName -> [Term] -> OpTable -> String
checkParenthese fName ts opt = if (needParenthese fName ts opt) 
    then (printOptFuncionTerm fName ts opt)
    else (printOptFuncionHeadWithoutParen fName ts opt)

-- To check if need parenthese We shoudld 
-- check if there is higher operation in Term list
-- If the higher function contain the next fun name in 
-- The list of Terms, there should be parenthese
needParenthese :: FName -> [Term] -> OpTable -> Bool
needParenthese fName ts opt = length higherfunctions /= length (higherfunctions \\ (getNextFunName (getFunctionNameFromTerms ts)))
    where higherfunctions = getHigherLevelFunction fName (getNameList opt) opt

-- A function a get all funName in [Term]
getFunctionNameFromTerms :: [Term] -> [FName]
getFunctionNameFromTerms [t] = convertTermToFName t
getFunctionNameFromTerms (t:ts) = (getFunctionNameFromTerms [t]) ++ (getFunctionNameFromTerms ts)

convertTermToFName :: Term -> [FName]
convertTermToFName (TVar v) = []
convertTermToFName (TNum i) = []
convertTermToFName (TFun fName ts) = [fName] ++ (getFunctionNameFromTerms ts)

getNextFunName :: [FName] -> [FName]
getNextFunName [] = []
getNextFunName [t] = [t]
getNextFunName (t:ts) = [t]


-- A function to get all higher operations in the Optable
getSameLevelFunction :: FName -> OpTable -> [FName]
getSameLevelFunction fName (OpTable [(_, names)]) = if fName `elem` names then names else []
getSameLevelFunction fName (OpTable (n:ns)) = (getSameLevelFunction fName (OpTable [n])) 
                                            ++ (getSameLevelFunction fName (OpTable ns)) 

-- A function to get higher level function names in the Optable 
getHigherLevelFunction :: FName -> [FName] -> OpTable -> [FName]
getHigherLevelFunction fName names opt = (take (fromJust (elemIndex fName names)) names) 
                                        \\ (getSameLevelFunction fName opt)