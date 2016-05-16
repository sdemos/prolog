module Prolog where

import Text.Megaparsec
import Text.Megaparsec.String

type Prolog = [Clause]
data Clause = Clause Pexp Pexps
type Pexps  = [Pexp]
data Pexp   = PExp Exp
            | PIs Exp Exp
            | PLess Exp Exp
            | PGreat Exp Exp
            | PLessEq Exp Exp
            | PGreatEq Exp Exp
            | PEq Exp Exp
            | PNotEq Exp Exp
data Exp    = ETerm Term
            | EPlus Exp Term
            | EMinus Exp Term
data Term   = TFactor Factor
            | TMult Term Factor
            | TDiv Term Factor
data Factor = FVar Var
            | FNum Integer
            | FId Id
            | FPred Id Pexps
            | FNeg Factor
            | FGroup Exp
type Var    = String
type Id     = String

program :: Parser Prolog
program = some clause

clause :: Parser Clause
clause = do
    p <- pexp
    space
    ps <- option [] (space >> string ":-" >> space >> pexps)
    space
    string "."
    space
    return (Clause p ps)

pexps :: Parser Pexps
pexps = sepBy1 pexp (space >> string "," >> space)

pexp :: Parser Pexp
pexp = try isPexp
   <|> try lessPexp
   <|> try greatPexp
   <|> try lessEqPexp
   <|> try greatEqPexp
   <|> try EqPexp
   <|> try NotEqPexp
   <|> PExp <$> exp

pexpOp :: String -> (Exp -> Exp -> Pexp) -> Parser Pexp
pexpOp op con = do
    e1 <- exp
    space
    string op
    space
    e2 <- exp
    return (con e1 e2)

isPexp      = pexpOp "is" PIs
lessPexp    = pexpOp "<"  PLess
greatPexp   = pexpOp ">"  PGreat
lessEqPexp  = pexpOp "<=" PLessEq
greatEqPexp = pexpOp ">=" PGreatEq
eqPexp      = pexpOp "="  PEq
notEqPexp   = pexpOp "\=" PNotEq

exp :: Parser Exp
exp = try plusExp
  <|> try minusExp
  <|> ETerm <$> term

expOp :: String -> (Exp -> Term -> Exp) -> Parser Exp
expOp op con = do
    e <- exp
    space
    string op
    space
    t <- term
    space
    return (con e t)

plusExp  = expOp "+" EPlus
minusExp = expOp "-" EMinus

term :: Parser Term
term = try multTerm
   <|> try divTerm
   <|> TFactor <$> factor

termOp :: String -> (Term -> Factor -> Term) -> Parser Term
termOp = op con = do
    t <- term
    space
    string op
    space
    f <- factor
    space
    return (con t f)

multTerm = termOp "*" TMult
divTerm  = termOp "/" TDiv

factor :: Parser Factor
factor = try var
     <|> try num
     <|> try pterm
     <|> try pterm0
     <|> try (FNeg <$> string "-" *> space *> factor)
     <|> try group

parens = between (string "(") (string ")")

-- vars start with an uppercase letter or an _
var = do
    s <- try upperChar <|> try (string "_")
    rest <- many alphaNumChar
    return $ FVar (s : rest)
-- ids start with a lowercase letter
ident = do
    s <- lowerChar
    rest <- many alphaNumChar
    return $ FId (s : rest)
pterm = FPred <$> ident <*> parens pexps
pterm0 = ident
group = parens exp
