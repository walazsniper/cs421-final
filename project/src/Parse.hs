-- | The parser goes here

module Parse where

import Prelude hiding (id)

import Types

import qualified Data.HashMap.Lazy as M

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Text.Parsec.Expr

--- The Parser
--- ----------

data ConCtx = ConCtx
  { conMap  :: M.HashMap Name (Int, Int)  -- ctor name -> (tag, arity)
  , nextTag :: Int                        -- next tag to hand out
  }

initialConCtx :: ConCtx
initialConCtx = ConCtx M.empty 1

type Parser = ParsecT String ConCtx Identity

--- ### Lexicals

reservedWords :: [String]
reservedWords = ["let", "letrec", "in", "case", "of", "Pack"]

isIdChar :: Char -> Bool
isIdChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_")

symbol :: String -> Parser String
symbol s = try $ do
  _ <- string s
  spaces
  return s

keyword :: String -> Parser ()
keyword s = try $ do
  _ <- string s
  notFollowedBy (satisfy isIdChar)
  spaces

int :: Parser Int
int = do
  digits <- many1 digit <?> "an integer"
  spaces
  return (read digits :: Int)

ident :: Parser Name
ident = try $ do
  first <- oneOf ['a' .. 'z']
  rest  <- many (satisfy isIdChar)
  let name = first : rest
  if name `elem` reservedWords
    then unexpected ("reserved word " ++ show name)
    else do
      spaces
      return name

conIdent :: Parser Name
conIdent = try $ do
  first <- oneOf ['A' .. 'Z']
  rest  <- many (satisfy isIdChar)
  let name = first : rest
  if name == "Pack"
    then unexpected "Pack keyword"
    else do
      spaces
      return name

identOrUnderscore :: Parser Name
identOrUnderscore = ident <|> (symbol "_" >> return "_")

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

braces :: Parser a -> Parser a
braces p = between (symbol "{") (symbol "}") p

angles :: Parser a -> Parser a
angles p = between (symbol "<") (symbol ">") p

--- ### Expressions

-- Atomic expression: variable, number, Pack literal, constructor reference,
-- or parenthesised expression.
aexpr :: Parser Expr
aexpr =  (ENum <$> int)
     <|> packLit
     <|> conRef
     <|> (EVar <$> ident)
     <|> parens expr
     <?> "atomic expression"

-- Pack{tag,arity}
packLit :: Parser Expr
packLit = try $ do
  keyword "Pack"
  braces $ do
    t <- int
    _ <- symbol ","
    a <- int
    return (EPack t a)

conRef :: Parser Expr
conRef = do
  name <- conIdent
  ctx  <- getState
  case M.lookup name (conMap ctx) of
    Just (t, a) -> return (EPack t a)
    Nothing     -> unexpected ("unknown constructor " ++ show name)

-- left-associative.
application :: Parser Expr
application = do
  f    <- aexpr
  args <- many aexpr
  return $ foldl EAp f args

expr :: Parser Expr
expr =  letExpr
    <|> caseExpr
    <|> lamExpr
    <|> binopExpr
    <?> "expression"

binopExpr :: Parser Expr
binopExpr = buildExpressionParser opTable application
  where
    opTable =
      [ [ infixOp "*"  AssocRight, infixOp "/"  AssocNone ]
      , [ infixOp "+"  AssocRight, infixOp "-"  AssocNone ]
      , [ infixOp "<=" AssocNone , infixOp ">=" AssocNone
        , infixOp "==" AssocNone , infixOp "~=" AssocNone
        , infixOp "<"  AssocNone , infixOp ">"  AssocNone ]
      , [ infixOp "&"  AssocRight ]
      , [ infixOp "|"  AssocRight ]
      ]

    infixOp name assoc = Infix (binOp name) assoc

    binOp :: String -> Parser (Expr -> Expr -> Expr)
    binOp s = try $ do
      _ <- string s
      case s of
        "<" -> notFollowedBy (char '=')
        ">" -> notFollowedBy (char '=')
        _   -> return ()
      spaces
      return (\a b -> EAp (EAp (EBinop s) a) b)

--- ### Compound expressions

letExpr :: Parser Expr
letExpr = do
  isRec <- (keyword "letrec" >> return True)
       <|> (keyword "let"    >> return False)
  ds <- defns
  keyword "in"
  body <- expr
  return $ ELet isRec ds body

defn :: Parser (Name, Expr)
defn = do
  n <- ident
  _ <- eqSym
  e <- expr
  return (n, e)

defns :: Parser [(Name, Expr)]
defns = defn `sepBy1` symbol ";"

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  as <- alts
  return $ ECase scrut as

alt :: Parser (Int, [Name], Expr)
alt = do
  t  <- angles int
  ps <- many identOrUnderscore
  _  <- symbol "->"
  e  <- expr
  return (t, ps, e)

alts :: Parser [(Int, [Name], Expr)]
alts = alt `sepBy1` symbol ";"

lamExpr :: Parser Expr
lamExpr = do
  _  <- symbol "\\"
  ps <- many1 ident
  _  <- symbol "."
  e  <- expr
  return $ ELam ps e

eqSym :: Parser ()
eqSym = try $ do
  _ <- char '='
  notFollowedBy (char '=')
  spaces

--- ### Type declarations

ctorArg :: Parser ()
ctorArg =  void (many1 (char '*') >> spaces)
       <|> void ident
       <|> void (parens (many ctorArg))

ctor :: Parser (Name, Int)
ctor = do
  n    <- conIdent
  args <- many ctorArg
  return (n, length args)

-- Parses `[* ...] ::= Cons1 ... | Cons2 ... | ...` and registers each
-- constructor with a fresh tag in ConCtx.  Returns () because type decls
-- do not contribute a Decl to the Core program
typeDeclTail :: Parser ()
typeDeclTail = do
  _  <- many (many1 (char '*') >> spaces)
  _  <- symbol "::="
  cs <- ctor `sepBy1` symbol "|"
  registerCtors cs

registerCtors :: [(Name, Int)] -> Parser ()
registerCtors cs = do
  ctx <- getState
  let go (m, t) (n, a) = (M.insert n (t, a) m, t + 1)
      (m', t')        = foldl go (conMap ctx, nextTag ctx) cs
  setState (ctx { conMap = m', nextTag = t' })

--- ### Top-level declarations

topDecl :: Parser (Maybe Decl)
topDecl = do
  name <- ident
  (typeDeclTail >> return Nothing) <|> (Just <$> scDeclTail name)

scDeclTail :: Name -> Parser Decl
scDeclTail name = do
  params <- many ident
  eqSym
  body <- expr
  return (name, params, body)

core :: Parser Core
core = do
  spaces
  decls <- topDecl `sepBy` symbol ";"
  eof
  return $ M.fromList [(n, d) | Just d@(n, _, _) <- decls]

parseCore :: String -> Either ParseError Core
parseCore = runParser core initialConCtx "Core"
