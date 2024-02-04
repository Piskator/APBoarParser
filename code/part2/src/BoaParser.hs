-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

import Data.Char 

type Parser a = ReadP a 

type ParseError = String -- you may replace this


parseString :: String -> Either ParseError Program
parseString input = case readP_to_S (do whitespaces; prog <- pProgram; eof; return prog) input of
                          [] -> Left "cannot parse"
                          [(a, "")] -> Right a -- the _ must be "", since 'eof' ok
                          _ -> error "Oops, my grammar is ambiguous!"


reserved :: [String]
reserved = ["while", "do", "None", "True", "False", "for", "if", "in", "not"]

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespaces; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

keyword :: String -> Parser ()
keyword s = lexeme $ do s' <- munch1 isAlphaNum
                        if s == s' then return () else pfail 

notFollowdBy :: Parser a -> Parser ()
notFollowdBy p = 
      do b <- (do p; return True) <++ return False 
         if b then pfail else return ()                                  

pComment :: Parser ()
pComment = do char '#'; munch (/= '\n'); munch (== '\n'); ws; return ()
               
ws :: Parser ()
ws = do munch isSpace; return ()

whitespaces :: Parser ()
whitespaces = do pComment; whitespaces
                <|> do ws 

pProgram :: Parser Program
pProgram = pStmts

pStmts :: Parser [Stmt]
pStmts = do stmt <- pStmt; stmtsOpt <- pStmtsOpt stmt; return stmtsOpt 

pStmtsOpt :: Stmt -> Parser [Stmt]
pStmtsOpt stmt =  
    do symbol ";"; newStmt <- pStmt; stmtsOpt <- pStmtsOpt newStmt; return $ [stmt] ++ stmtsOpt
    <|> return [stmt]

pStmt :: Parser Stmt 
pStmt = do exp <- pNotExp; return $ SExp exp
    <|> do ident <- pLexemeIdent; symbol "="; exp <- pNotExp; return $ SDef ident exp

pNotExp :: Parser Exp 
pNotExp  = 
        do keyword "not"; notExp <- pNotExp; return $ Not notExp
    <|> do exp <- pExp; return exp

pExp :: Parser Exp
pExp = do term <- pTerm; eRelOpt <- pERelOpt term; return eRelOpt

{- Note that the if statement i necessary to avoid an infinite loop. 
   As seen in the re-written grammer it the EOpt returned is only
   occuring if none the other symbols are registered -}
pERelOpt :: Exp -> Parser Exp 
pERelOpt exp = 
        do symbol "=="; term <- pTerm; eOpt <- pEOpt term; return $ Oper Eq exp eOpt
    <|> do symbol "!="; term <- pTerm; eOpt <- pEOpt term; return $ Not (Oper Eq exp eOpt)
    <|> do symbol "<"; term <- pTerm; eOpt <- pEOpt term; return $ Oper Less exp eOpt
    <|> do symbol "<="; term <- pTerm; eOpt <- pEOpt term; return $ Not (Oper Greater exp eOpt)
    <|> do symbol ">"; term <- pTerm; eOpt <- pEOpt term; return $ Oper Greater exp eOpt
    <|> do symbol ">="; term <- pTerm; eOpt <- pEOpt term; return $ Not (Oper Less exp eOpt)
    <|> do keyword "in"; term <- pTerm; eOpt <- pEOpt term; return $ Oper In exp eOpt    
    <|> do keyword "not"; keyword "in"; term <- pTerm; eOpt <- pEOpt term; return $ Not (Oper In exp eOpt) -- Notice use of two keywords. In our shared solution it is uses for both list and Var
    <|> do eOpt <- pEOpt exp; 
            if eOpt == exp then return exp 
                           else do eRelOpt <- pERelOpt eOpt; return eRelOpt

pEOpt :: Exp -> Parser Exp 
pEOpt exp = 
        do symbol "+"; term <- pTerm; return $ Oper Plus exp term
    <|> do symbol "-"; term <- pTerm; return $ Oper Minus exp term
    <|> return exp

pTerm :: Parser Exp 
pTerm = do factor <- pFactor; termOpt <- pTermOpt factor; return termOpt

{- Notice how this exmplifies the notification in the above grammar. -}
pTermOpt :: Exp -> Parser Exp
pTermOpt exp = 
        do symbol "*"; factor <- pFactor; termOpt <- pTermOpt (Oper Times exp factor);  return $ termOpt
    <|> do symbol "//"; factor <- pFactor; termOpt <- pTermOpt (Oper Div exp factor);  return $ termOpt
    <|> do symbol "%"; factor <- pFactor; termOpt <- pTermOpt (Oper Mod exp factor);  return $ termOpt
    <|> do return exp

pFactor :: Parser Exp 
pFactor = pLexicals <|> pBool    
    <|> do ident <- pLexemeIdent; return $ Var ident
    <|> do ident <- pLexemeIdent; symbol "("; exprz <- pExprz; symbol ")"; return $ Call ident exprz
    <|> do symbol "("; exp <- pNotExp; symbol ")"; return exp        
    <|> pNestedExpr
    
pNestedExpr :: Parser Exp
pNestedExpr = do
    symbol "["
    choice
        [ do
            symbol "]"
            return $ List []
        , do
            exp <- pNotExp
            parseListOrCompr exp
        ]

parseListOrCompr :: Exp -> Parser Exp
parseListOrCompr exp = choice
    [ do
        exprz <- pExprsOpt exp
        symbol "]"
        return $ List exprz
    , do
        forCl <- pForClause
        clausez <- pClausez
        symbol "]"
        return $ Compr exp (forCl : clausez)
    ]

pBool :: Parser Exp 
pBool = 
        do keyword "None"; return $ Const NoneVal
    <|> do keyword "True"; return $ Const TrueVal
    <|> do keyword "False"; return $ Const FalseVal

pForClause :: Parser CClause 
pForClause = do keyword "for"; ident <- pLexemeIdent; keyword "in"; exp <- pNotExp; return $ CCFor ident exp

pIfClause :: Parser CClause
pIfClause = do keyword "if"; exp <- pNotExp; return $ CCIf exp 

pClausez :: Parser [CClause]
pClausez = 
        do fCl <- pForClause;  clausez <- pClausez; return $ (fCl:clausez)
    <|> do ifClause <- pIfClause; clausez <- pClausez; return $ (ifClause:clausez)    
    <|> do return []    
    
pExprz :: Parser [Exp] 
pExprz = 
        do exprs <- pExprs; return $ exprs
    <|> do return [] 

pExprs :: Parser [Exp]
pExprs = do exp <- pNotExp; exprsOpt <- pExprsOpt exp; return $ exprsOpt

pExprsOpt :: Exp -> Parser [Exp]
pExprsOpt exp = 
    do symbol ","; exp2 <- pNotExp; exprsOpt <- pExprsOpt exp2; return (exp:exprsOpt)
    <|> return [exp] 

pLexicals :: Parser Exp
pLexicals = lexeme (pStringConst <|> pNum)

--Check overlapping begining with keywords and variables for instance
pStringConst :: Parser Exp
pStringConst = do p <- pString; return (Const (StringVal p))

pString :: Parser String
pString = do (char '\''); s <- (many pChar); (char '\''); return $ concat s 

pChar :: Parser String
pChar = do c <- (satisfy (\x -> isAscii x && isPrint x && x /= '\\' && x /= '\'')); return [c]
    <|> do satisfy (== '\\'); satisfy (== '\\'); return "\\"
    <|> do satisfy (== '\\'); (char 'n'); return "\n"
    <|> do satisfy (== '\\'); (char '\n'); return ""    
    <|> do satisfy (== '\\'); (char '\''); return "\'"

{- TODO: (No whitespace is needed after a numeric constant, unless it is
immediately followed by another digit.) -}
pNum :: Parser Exp
pNum = do n <- pNumConst; return $ Const (IntVal n)

pNumConst :: Parser Int
pNumConst = 
        do char '0' <|> (do char '-'; satisfy (== '0')); return 0
    <|> do char '-'; c <- satisfy (\x -> isDigit x && x /= '0'); cs <- munch isDigit; return $ -1 * (read ([c] ++ cs))
    <|> do c <- satisfy (\x -> isDigit x && x /= '0'); cs <- munch isDigit; return $ read ([c] ++ cs)
    
pLexemeIdent :: Parser [Char] 
pLexemeIdent = lexeme pIdent

pIdent :: Parser [Char] 
pIdent = do c <- satisfy (\x -> isLetter x || x == '_'); cs <- munch (\x -> isLetter x || isDigit x || x == '_'); 
            if ([c] ++ cs) `elem` reserved then pfail else return $ ([c] ++ cs)


{- Re-rewritten grammar

Program ::= Stmts
Stmts ::= Stmt StmtsOpt
StmtsOpt :=   ";" Stmts | eps 
Stmt::=  NotE | ident "=" NotE //none of these two are self left recursive.

NotExpr::= "not" NotExpr | Expr

E    ::= T ERelopt 
ERelOpt ::= "==" T EOpt | EOpt ERelOpt | eps   -- Notice that "==" can't occur more than once since ERelOpt is only called when the other isn't
EOpt ::= "+" T Eopt | "-" T EOpt | eps
T    ::= F TOpt 
TOpt ::= "*" T | "/" T | eps    --Notice that even though T is equal to F TOpt we use the latter in this lines ince only this is in need of recursive call. And we need to have a certain acosiativity why a certain return Oper also is needed.
F    ::= Real | '(' E ')'

F :: = numConst | stringConst | 'None' .. 'False' | ident | '(' Expr ')' | ident ‘(’ Exprz ‘)’ |‘[’ Exprz ‘]’ |‘[’ Expr ForClause Clausez ‘]’

TermRel ::= TermAdd TermRelOpt 

ForClause ::= ‘for’ ident ‘in’ Expr
IfClause ::= ‘if’ Expr

Clausez ::= Eps | ForClause Clausez | IfClause Clausez

Exprz = eps | Exprs
Exprs ::= Expr ExprsOpt
ExprsOpt ::=  "," Exprs | eps

ident ::= See text
numConst ::= See text
stringConst :: = See text

1 + 2 * 3 - 5 + 6 * 7
Add (Sub (Add 1 (Mul 2 3) - 5) (Mul 6 7))
(Add Four (Mul Four Three))


-}

{- Playing with association

(1 + (2 * 3) + (4 * 5*6)) 

1 + 2 * 3 - 5 + 6 * 7
Add (Sub (Add 1 (Mul 2 3) - 5) (Mul 6 7))
(Add Four (Mul Four Three))

 -}