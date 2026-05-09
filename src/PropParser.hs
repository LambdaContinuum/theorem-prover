module PropParser where

import PropLanguage
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

sc :: Parser ()
sc = L.space space1
             (L.skipLineComment "--")
             (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: [String]
reserved = ["true", "false", "∀", "∃"]

checkReserved :: String -> Parser String
checkReserved name
  | name `elem` reserved =
      fail $ "'" ++ name ++ "' is a reserved keyword and cannot be used as an identifier"
  | otherwise = return name

lowerIdent :: Parser String
lowerIdent = label "lowercase identifier" . lexeme $ do
  c  <- lowerChar
  cs <- many alphaNumChar
  checkReserved (c:cs)

upperIdent :: Parser String
upperIdent = label "uppercase identifier" . lexeme $ do
  c  <- upperChar
  cs <- many alphaNumChar
  checkReserved (c:cs)

termVar :: Parser Term
termVar = label "term variable (e.g. x, y)" $
  TVar <$> lowerIdent

termFunc :: Parser Term
termFunc = label "function application (e.g. f(x, y))" $ do
  name <- lowerIdent
  args <- parens (term `sepBy` symbol ",")
  return (TFunc name args)

term :: Parser Term
term = label "term" $ try termFunc <|> termVar

boolLit :: Parser Prop
boolLit = label "'true' or 'false'" $
      (Con True  <$ symbol "true")
  <|> (Con False <$ symbol "false")

propVar :: Parser Prop
propVar = label "propositional variable (e.g. P, MyProp)" $ do
  name <- upperIdent
  notFollowedBy (char '(') <?> "no argument list — did you mean a predicate like P(x)?"
  return (Var name)

predicate :: Parser Prop
predicate = label "predicate (e.g. Loves(x, y))" $ do
  name <- upperIdent
  args <- parens (term `sepBy` symbol ",") <?> "argument list"
  return (Pred name args)

quantifier :: Parser Prop
quantifier = label "quantifier (forall/exists)" $ do
  ctor <- (Forall <$ symbol "∀") <|> (Exists <$ symbol "∃")
  var  <- lowerIdent <?> "bound variable (must be lowercase)"
  _    <- symbol "."  <?> "'.' after bound variable"
  body <- prop        <?> "proposition body after '.'"
  return (ctor var body)

atomProp :: Parser Prop
atomProp = label "proposition" $ choice
  [ parens prop
  , boolLit
  , quantifier
  , try predicate <|> propVar
  ]

prop :: Parser Prop
prop = makeExprParser atomProp operatorTable

operatorTable :: [[Operator Parser Prop]]
operatorTable =
  [ [ Prefix  (foldr1 (.) <$> some (Not <$ symbol "¬") <?> "negation (¬)") ] 
  , [ InfixL  (And     <$ (symbol "∧"   <?> "conjunction (∧)")) ]
  , [ InfixL  (Or      <$ (symbol "∨"   <?> "disjunction (∨)")) ]
  , [ InfixR  (Implies <$ (symbol "→"  <?> "implication (→)")) ]
  , [ InfixR  (Iff     <$ (symbol "↔" <?> "biconditional (↔)")) ]
  ]

parseProp :: String -> Either (ParseErrorBundle String Void) Prop
parseProp input = parse (sc *> prop <* eof) "<input>" input

parseTerm :: String -> Either (ParseErrorBundle String Void) Term
parseTerm input = parse (sc *> term <* eof) "<input>" input
