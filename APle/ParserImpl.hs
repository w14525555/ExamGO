module ParserImpl where

import AST
import Text.ParserCombinators.ReadP

-- Grammar

-- Term ::= vname | number | fname ‘(’ Termz ‘)’ | Term oper Term | ‘(’ Term ‘)’
-- Termz ::= e | Terms
-- Terms ::= Term | Term ‘,’ Terms
-- Cond ::= pname ‘(’ Termz ‘)’ | pname ‘(’ Termz ‘;’ Terms ‘)’
-- Conds ::= Cond | Cond ‘,’ Conds
-- Rule ::= Term ‘=’ Term ‘.’ | Term ‘=’ Term ‘|’ Conds ‘.’
-- Cmd ::= Rule | Term ‘?’ | Term ‘??’
-- Cmds ::= e | Cmd Cmds

-- Utility Functions

-- characters from a - Z
characters :: String
characters = ['a'..'z'] ++ ['A'..'Z']

digit :: String
digit = ['0'..'9']

operators :: String
operators = ['!', '@', '#', '+', '-', '*', '/', '\\', '<', '>', '=']

-- A function make code cleaner
token :: ReadP a -> ReadP a
token p = skipSpaces >> p

-- A function accept fname term term
-- returns a function term
toFunction :: FName -> Term -> Term -> Term
toFunction fname t1 t2 = TFun fname [t1, t2]

-- API Functions

-- do not change the type!
parseStringTerm :: OpTable -> String -> Either ErrMsg Term
parseStringTerm opt input = case readP_to_S (parseOptTerm opt opt <* eof) input of 
    [(result, [])] -> Right result
    [] -> Left "empty result probably with wrong intput"
    a:b:cs -> Left "ambiguous error"

parseStringCmds :: OpTable -> String -> Either ErrMsg [Cmd]
parseStringCmds opt input = case readP_to_S (parseOptCmds opt <* eof) input of 
     [(result, [])] -> Right result
     [] -> Left "empty result probably with wrong intput"
     a:b:cs -> Left "ambiguous error"

-- Internal Functions

type OpTableElement = (Fixity, [FName])

--- Opt functions

-- Parse Term
parseOptTerm :: OpTable -> OpTable -> ReadP Term
parseOptTerm (OpTable [o]) opt = parseOptItem o opt
parseOptTerm (OpTable ((FNone, fnames):os)) opt = do
    chainl1 (parseOptTerm (OpTable os) opt) (combineOperations fnames)
parseOptTerm (OpTable ((FLeft, fnames):os)) opt = do
    chainl1 (parseOptTerm (OpTable os) opt) (combineOperations fnames)
parseOptTerm (OpTable ((FRight, fnames):os)) opt = do
    chainr1 (parseOptTerm (OpTable os) opt) (combineOperations fnames)

parseOptItem :: OpTableElement -> OpTable -> ReadP Term
parseOptItem (FNone, names) opt = parseOptableNoneOrLeft names opt
parseOptItem (FLeft, names) opt = parseOptableNoneOrLeft names opt
parseOptItem (FRight, names) opt = parseOptableRight names opt

-- Handle the case of Fixity = FLeft | FNone
parseOptableNoneOrLeft :: [FName] -> OpTable -> ReadP Term
parseOptableNoneOrLeft fnames opt = do 
    chainl1 (parseBottomTerms opt) (combineOperations fnames)

-- Handle the case of Fixity = FRight
parseOptableRight :: [FName] -> OpTable -> ReadP Term
parseOptableRight fnames opt = do 
    chainr1 (parseBottomTerms opt) (combineOperations fnames) 

-- Combine the operations 
combineOperations :: [FName] -> ReadP (Term -> Term -> Term)
combineOperations [n] =  (operations n)
combineOperations (n:ns) = (combineOperations [n]) +++ (combineOperations ns)

-- A general function accept the fname and return a functions name
operations :: FName -> ReadP (Term -> Term -> Term)
operations fname = do
    opt <- string fname
    return (toFunction opt)

-- Parse Cmds
-- Cmds ::= e | Cmd Cmds
parseOptCmds :: OpTable -> ReadP [Cmd]
parseOptCmds opt = (parseNonEmptyCmds opt) +++ (token $ return [])

parseNonEmptyCmds :: OpTable -> ReadP [Cmd]
parseNonEmptyCmds opt = do
    c <- parseCmd opt
    cs <- parseOptCmds opt
    return $ c:cs

-- Cmd ::= Rule | Term ‘?’ | Term ‘??’
parseCmd :: OpTable -> ReadP Cmd
parseCmd opt = (parseRuleCmd opt) +++ (parseFlagCmd opt)

parseRuleCmd :: OpTable -> ReadP Cmd
parseRuleCmd opt = do
    r <- parseRule opt
    return $ CRule r

parseFlagCmd :: OpTable -> ReadP Cmd
parseFlagCmd opt = do
    t <- parseOptTerm opt opt
    flag <- parseFlag
    token $ return $ CQuery t flag

parseFlag :: ReadP Bool
parseFlag = do
    f <- many1 (satisfy (`elem` ['?']))
    if f == "?" 
        then return False 
        else if f == "??" 
            then return True 
            else pfail

-- Parse Rule
-- Rule ::= Term ‘=’ Term ‘.’ | Term ‘=’ Term ‘|’ Conds ‘.’
parseRule :: OpTable -> ReadP Rule
parseRule opt = (parseTermTermDot opt) +++ (parseTermTermConds opt)

parseTermTermDot :: OpTable -> ReadP Rule
parseTermTermDot opt = do
    t1 <- parseOptTerm opt opt
    _ <- char '='
    t2 <- parseOptTerm opt opt
    _ <- char '.'
    token $ return $ Rule t1 t2 []

parseTermTermConds :: OpTable -> ReadP Rule
parseTermTermConds opt = do
    t1 <- parseOptTerm opt opt
    _ <- char '='
    t2 <- parseOptTerm opt opt
    _ <- char '|'
    cs <- parseConds opt
    _ <- char '.'
    token $ return $ Rule t1 t2 cs

-- Conds ::= Cond | Cond ‘,’ Conds
parseConds :: OpTable -> ReadP [Cond]
parseConds opt = (parseSingleCond opt) +++ (parseCommaConds opt)

parseSingleCond :: OpTable -> ReadP [Cond]
parseSingleCond opt = do
    c <- parseCond opt
    return [c]

parseCommaConds :: OpTable -> ReadP [Cond]
parseCommaConds opt = do
    c <- parseCond opt
    _ <- char ','
    cs <- parseConds opt
    return $ c:cs

-- Parse Condition
-- Cond ::= pname ‘(’ Termz ‘)’ | pname ‘(’ Termz ‘;’ Terms ‘)’
parseCond :: OpTable -> ReadP Cond
parseCond opt = do
    pname <- parseVFPNames
    _ <- char '('
    ts1 <- parseTermZ opt
    ts2 <- parseSecondPartCond opt
    _ <- char ')'
    token $ return $ Cond pname ts1 ts2

parseSecondPartCond :: OpTable -> ReadP [Term]
parseSecondPartCond opt = (parseSecondTerms opt) +++ (return [])

parseSecondTerms :: OpTable -> ReadP [Term]
parseSecondTerms opt = do
    _ <- char ';'
    ts <- parseTerms opt
    return ts

-- Term ::= vname | number | fname ‘(’ Termz ‘)’ | ‘(’ Term ‘)’
parseBottomTerms :: OpTable -> ReadP Term
parseBottomTerms opt = parseNumberTerm +++ parseVNameTerm +++ (parseFuncTerm opt) +++ (parseParentheseTerm opt)

--Bottom parsers
parseNumberTerm :: ReadP Term
parseNumberTerm = do
    n <- parseNumber
    return $ TNum n

parseVNameTerm :: ReadP Term
parseVNameTerm = do
    vname <- parseVFPNames
    return $ TVar vname

parseFuncTerm :: OpTable -> ReadP Term
parseFuncTerm opt = do
    fname <- parseVFPNames
    _ <- char '('
    terms <- parseTermZ opt
    _ <- char ')'
    token $ return $ TFun fname terms

parseTermZ :: OpTable -> ReadP [Term]
parseTermZ opt = (parseTerms opt) +++ (token (return []))

parseTerms :: OpTable -> ReadP [Term]
parseTerms opt = (parseSingleTerm opt) +++ (parseCommaTerms opt)

parseSingleTerm :: OpTable -> ReadP [Term]
parseSingleTerm opt = do
    t <- parseOptTerm opt opt
    return [t]

parseCommaTerms :: OpTable -> ReadP [Term]
parseCommaTerms opt = do
    t <- parseOptTerm opt opt
    _ <- char ','
    ts <- parseTerms opt
    return $ t:ts

parseParentheseTerm :: OpTable -> ReadP Term
parseParentheseTerm opt = do
    _ <- token $ char '('
    t <- parseOptTerm opt opt
    _ <- char ')'
    token $ return t

-- Parser for VFP names
parseVFPNames :: ReadP String
parseVFPNames = do
    c <- token $ satisfy (`elem` characters)
    cs <- munch (`elem` characters ++ digit)
    token $ return $ [c] ++ cs

-- Parse number including positive and negative numbers
parseNumber :: ReadP Integer
parseNumber = parsePositiveNum +++ parseNegtiveNumber

parsePositiveNum :: ReadP Integer
parsePositiveNum = do
    ns <- token $ many1 (satisfy (`elem` digit))
    token $ return (read ns :: Integer)

parseNegtiveNumber :: ReadP Integer
parseNegtiveNumber = do
    _ <- token $ char '~'
    ns <- many1 (satisfy (`elem` digit))
    token $ return (read ("-" ++ ns) :: Integer)