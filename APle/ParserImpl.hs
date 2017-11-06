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
parseStringTerm opt input = case readP_to_S (parseOptTerm opt <* eof) input of 
    [(result, [])] -> Right result
    [] -> Left "empty result probably with wrong intput"
    a:b:cs -> Left "ambiguous error"

parseStringCmds :: OpTable -> String -> Either ErrMsg [Cmd]
parseStringCmds opt input = case readP_to_S (parseCmds <* eof) input of 
    [(result, [])] -> Right result
    [] -> Left "empty result probably with wrong intput"
    a:b:cs -> Left "ambiguous error"

-- An API used to test Cmds
parseCmdsWithoutOpTable :: String -> Either ErrMsg [Cmd]
parseCmdsWithoutOpTable input = case readP_to_S (parseCmds <* eof) input of 
    [(result, [])] -> Right result
    [] -> Left "empty result probably with wrong intput"
    a:b:cs -> Left "ambiguous error"

-- Internal Functions

type OpTableItem = (Fixity, [FName])

--- Opt functions
-- Parse Term
parseOptTerm :: OpTable -> ReadP Term
parseOptTerm (OpTable [o]) = parseOptItem o
parseOptTerm (OpTable ((FNone, fnames):os)) = do
    chainl1 (parseOptTerm (OpTable os)) (combineOperations fnames)
parseOptTerm (OpTable ((FLeft, fnames):os)) = do
    chainl1 (parseOptTerm (OpTable os)) (combineOperations fnames)
parseOptTerm (OpTable ((FRight, fnames):os)) = do
    chainr1 (parseOptTerm (OpTable os)) (combineOperations fnames)

parseOptItem :: OpTableItem -> ReadP Term
parseOptItem (FNone, names) = parseOptableNoneOrLeft names
parseOptItem (FLeft, names) = parseOptableNoneOrLeft names
parseOptItem (FRight, names) = parseOptableRight names

-- Handle the case of Fixity = FLeft | FNone
parseOptableNoneOrLeft :: [FName] -> ReadP Term
parseOptableNoneOrLeft fnames = do 
    chainl1 parseBottomTerms (combineOperations fnames)

-- Handle the case of Fixity = FRight
parseOptableRight :: [FName] -> ReadP Term
parseOptableRight fnames = do 
    chainr1 parseBottomTerms (combineOperations fnames) 

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
parseCmds :: ReadP [Cmd]
parseCmds = parseNonEmptyCmds +++ (token $ return [])

parseNonEmptyCmds :: ReadP [Cmd]
parseNonEmptyCmds = do
    c <- parseCmd
    cs <- parseCmds
    return $ c:cs

parseCmd :: ReadP Cmd
parseCmd = parseRuleCmd +++ parseFlagCmd

parseRuleCmd :: ReadP Cmd
parseRuleCmd = do
    r <- parseRule
    return $ CRule r

parseFlagCmd :: ReadP Cmd
parseFlagCmd = do
    t <- parseTerm
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
parseRule :: ReadP Rule
parseRule = parseTermTermDot +++ parseTermTermConds

parseTermTermDot :: ReadP Rule
parseTermTermDot = do
    t1 <- parseTerm
    _ <- char '='
    t2 <- parseTerm
    _ <- char '.'
    token $ return $ Rule t1 t2 []

parseTermTermConds :: ReadP Rule
parseTermTermConds = do
    t1 <- parseTerm
    _ <- char '='
    t2 <- parseTerm
    _ <- char '|'
    cs <- parseConds
    _ <- char '.'
    token $ return $ Rule t1 t2 cs

parseConds :: ReadP [Cond]
parseConds =  parseSingleCond +++ parseCommaConds

parseSingleCond :: ReadP [Cond]
parseSingleCond = do
    c <- parseCond
    return [c]

parseCommaConds :: ReadP [Cond]
parseCommaConds = do
    c <- parseCond
    _ <- char ','
    cs <- parseConds
    return $ c:cs

-- Parse Condition
parseCond :: ReadP Cond
parseCond = do
    pname <- parseVFPNames
    _ <- char '('
    ts1 <- parseTermZ
    ts2 <- parseSecondPartCond
    _ <- char ')'
    token $ return $ Cond pname ts1 ts2

parseSecondPartCond :: ReadP [Term]
parseSecondPartCond = parseSecondTerms +++ (return [])

parseSecondTerms :: ReadP [Term]
parseSecondTerms = do
    _ <- char ';'
    ts <- parseTerms
    return ts

-- Parse Term
-- Hard Coded Operators 
parseTerm :: ReadP Term
parseTerm = do
    chainl1 parseTerm1 topPrecedentOp

-- Hard Coded Operators 
parseTerm1 :: ReadP Term
parseTerm1 = do
    chainl1 parseTerm2 secondPrecedentOp

parseTerm2 :: ReadP Term
parseTerm2 = do
    chainl1 parseTerm3 thirdPrecedentOp

parseTerm3 :: ReadP Term
parseTerm3 = do
    chainr1 parseBottomTerms fourthPrecedentOp

parseBottomTerms :: ReadP Term
parseBottomTerms = parseNumberTerm +++ parseVNameTerm +++ parseFuncTerm +++ parseParentheseTerm

--Bottom parsers
parseNumberTerm :: ReadP Term
parseNumberTerm = do
    n <- parseNumber
    return $ TNum n

parseVNameTerm :: ReadP Term
parseVNameTerm = do
    vname <- parseVFPNames
    return $ TVar vname

parseFuncTerm :: ReadP Term
parseFuncTerm = do
    fname <- parseVFPNames
    _ <- char '('
    terms <- parseTermZ
    _ <- char ')'
    skipSpaces
    return $ TFun fname terms

parseTermZ :: ReadP [Term]
parseTermZ = parseTerms +++ (token (return []))

parseTerms :: ReadP [Term]
parseTerms = parseSingleTerm +++ parseCommaTerms

parseSingleTerm :: ReadP [Term]
parseSingleTerm = do
    t <- parseTerm
    return [t]

parseCommaTerms :: ReadP [Term]
parseCommaTerms = do
    t <- parseTerm
    _ <- char ','
    ts <- parseTerms
    return $ t:ts

parseParentheseTerm :: ReadP Term
parseParentheseTerm = do
    _ <- token $ char '('
    t <- parseTerm
    _ <- char ')'
    skipSpaces
    return t

topPrecedentOp :: ReadP (Term -> Term -> Term)
topPrecedentOp = lessEqual +++ less

secondPrecedentOp :: ReadP (Term -> Term -> Term)
secondPrecedentOp = plus +++ minus

thirdPrecedentOp :: ReadP (Term -> Term -> Term)
thirdPrecedentOp = multiply

fourthPrecedentOp :: ReadP (Term -> Term -> Term)
fourthPrecedentOp = doubleStar

plus :: ReadP (Term -> Term -> Term)
plus = do
    opt <- string "+"
    return (toFunction opt)

minus :: ReadP (Term -> Term -> Term)
minus = do
    opt <- string "-"
    return (toFunction opt)

lessEqual :: ReadP (Term -> Term -> Term)
lessEqual = do
    opt <- string "<="
    return (toFunction opt)

less :: ReadP (Term -> Term -> Term)
less = do
    opt <- string "<"
    return (toFunction opt)

multiply :: ReadP (Term -> Term -> Term)
multiply = do
    opt <- string "*"
    return (toFunction opt)

doubleStar :: ReadP (Term -> Term -> Term)
doubleStar = do
    opt <- string "**"
    return (toFunction opt)   

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

-- Parse Operators
parseOper :: ReadP String
parseOper = do
    ops <- token $ many1 (satisfy (`elem` operators))
    if ops == "=" then pfail else token $ return ops