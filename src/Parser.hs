-- Quick and dirty

module Parser where

import Text.Parsec hiding (token)
import Text.Parsec.String
import AST
import qualified Types as T
import Documentation.SBV.Examples.Queries.FourFours (UnOp)

cSpecSymbs :: String
cSpecSymbs      = "(){}[];,"
cIdentBegin :: [Char]
cIdentBegin     = '_':['a'..'z'] ++ ['A'..'Z']
cIdentChar :: [Char]
cIdentChar      = cIdentBegin ++ ['0'..'9']
cKeywords :: [String]
cKeywords       = ["if","while","return","else"]
cTypes :: [String]
cTypes          = ["int", "char"]
strLitChar :: [Char]
strLitChar = ' ':'!' : ['#'..'&'] ++ ['('..'~']

-- its useful trust me

leftRec :: (Stream s m t)
        => ParsecT s u m a -> ParsecT s u m (a -> a) -> ParsecT s u m a
leftRec p op = rest =<< p where
    rest x = do f <- op
                rest (f x)
            <|> return x

-- taken from my scheme parser, might be useful

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

token :: String -> Parser String
token w = lexeme $ string w

manyTill' :: Parser a -> Parser b -> Parser [a]
manyTill' p1 p2 = try ((:) <$> p1 <*> manyTill' p1 p2) <|> ([] <$ p2)

sat :: (Show a) => (a -> Bool) -> Parser a -> Parser a
sat p parser = do
  v <- parser
  if p v then return v else fail ("not expecting " ++ show v)


-- getting rid of comments (taken from jumcc)

preProc :: Parser String
preProc = do
    result <- many (try singleLineComment
                <|> try (spaces *> multiLineComment <* spaces)
                <|> try (do
                    c <- anyChar
                    return [c]))
    return $ concat result

singleLineComment :: Parser String
singleLineComment = string "//" <* manyTill anyChar (try $ char '\n')

multiLineComment :: Parser String
multiLineComment =  string "/*" <* manyTill anyChar (try $ string "*/" <* spaces)

-- between wrappers

parend :: Parser a -> Parser a
parend = between (token "(") (token ")")

brackd :: Parser a -> Parser a
brackd = between (token "[") (token "]")

braced :: Parser a -> Parser a
braced = between (token "{") (token "}")

squoted :: Parser a -> Parser a
squoted = between (char '\'') (token "'")

dquoted :: Parser a -> Parser a
dquoted = between (char '"') (token "\"")

-- basic stuff

cChar :: Parser Char
cChar = try ('\0' <$ string "\\0")
        <|> try ('\n' <$ string "\\n")
        <|> try ('"' <$ string "\\\"")
        <|> try ('\'' <$ string "\'")
        <|> try (oneOf strLitChar)

intLiteral :: Parser Integer
intLiteral = lexeme $ read <$> ((++) <$> option "" (string "-") <*> many1 digit)

charLiteral :: Parser Char
charLiteral = squoted cChar

strLiteral :: Parser String
strLiteral = lexeme $ dquoted (many cChar)

identifier :: Parser String
identifier = many1 (oneOf cIdentChar) <* spaces

ty :: Parser T.Type
ty = try (leftRec b ptr)
    <|> try b
    where
        ptr = T.Integral . T.Ptr <$ token "*"
        b =   try (T.Integral T.U32 <$ token "unsigned")
          <|> try (T.Integral T.Int32 <$ token "int")
          <|> try (T.Integral T.Int8 <$ token "char")
-- Exprs

primaryExpr :: Parser Expr
primaryExpr =   try (Var <$> identifier)
            <|> try (Int <$> intLiteral)
            <|> try (Char <$> charLiteral)
            <|> try (Str <$> strLiteral)
            <|> try (parend expression)

postfixExpr :: Parser Expr
postfixExpr =   try primaryExpr
            <|> try (Index <$> identifier <*> many1 (brackd expression))
            <|> try (FunCall <$> identifier <*> parend (sepBy expression (token ",")))
                  -- decrement, increment TODO

unaryExpr :: Parser Expr
unaryExpr =   try postfixExpr
          <|> try (UnExpr AST.LNot <$> (token "!" *> postfixExpr))
          <|> try (UnExpr AST.Neg <$> (token "-" *> postfixExpr))
          <|> try (UnExpr AST.BNot <$> (token "~" *> postfixExpr))

multiplicativeExpr :: Parser Expr
multiplicativeExpr =  try unaryExpr
                  <|> do
                    e1 <- multiplicativeExpr
                    op <- (Mul <$ token "*")
                      <|> (Div <$ token "/")
                      <|> (Mod <$ token "%")
                    BinExpr e1 op <$> unaryExpr

additiveExpr :: Parser Expr
additiveExpr =  try multiplicativeExpr
            <|> do
              e1 <- additiveExpr
              op <- (Add <$ token "+")
                <|> (Sub <$ token "-")
              BinExpr e1 op <$> multiplicativeExpr

shiftExpr :: Parser Expr
shiftExpr = try additiveExpr
        <|> do
          e1 <- shiftExpr
          op <- (Shl <$ token "<<")
            <|> (Shr <$ token ">>")
          ShiftExpr e1 op <$> additiveExpr

relationalExpr :: Parser Expr
relationalExpr =  try shiftExpr
              <|> do
                e1 <- relationalExpr
                op <- try (Leq <$ token "<=")
                  <|> try (Geq <$ token ">=")
                  <|> (Lt <$ token "<")
                  <|> (Gt <$ token ">")
                RelExpr e1 op <$> shiftExpr

equalityExpr :: Parser Expr
equalityExpr =  try relationalExpr
            <|> do
              e1 <- equalityExpr
              op <- try (Eq <$ token "==")
                <|> try (Neq <$ token "!=")
              RelExpr e1 op <$> relationalExpr

bitwiseAndExpr :: Parser Expr
bitwiseAndExpr =  try equalityExpr
              <|> BinExpr <$> bitwiseAndExpr <*> (BAnd <$ token "&") <*> equalityExpr

exclusiveOrExpr :: Parser Expr
exclusiveOrExpr = try bitwiseAndExpr
              <|> BinExpr <$> exclusiveOrExpr <*> (BXor <$ token "^") <*> bitwiseAndExpr

bitwiseOrExpr :: Parser Expr
bitwiseOrExpr = try exclusiveOrExpr
            <|> BinExpr <$> bitwiseOrExpr <*> (BXor <$ token "^") <*> exclusiveOrExpr

logicalAndExpr :: Parser Expr
logicalAndExpr =  try bitwiseOrExpr
              <|> LogExpr <$> logicalAndExpr <*> (LOr <$ token "&&") <*> bitwiseOrExpr

logicalOrExpr :: Parser Expr
logicalOrExpr = try logicalAndExpr
            <|> LogExpr <$> logicalOrExpr <*> (LOr <$ token "||") <*> logicalAndExpr

conditionalExpr :: Parser Expr
conditionalExpr = logicalOrExpr
                -- choice TODO

assignmentExpr :: Parser Expr
assignmentExpr =  try conditionalExpr
              <|> AssignExpr <$> unaryExpr <*> (token "=" *> assignmentExpr)

expression :: Parser Expr
expression = assignmentExpr

-- statements

statement :: Parser Stmt
statement =   try compoundStatement
          <|> try (IfElse <$> (token "if" *> parend expression) <*> compoundStatement <*> return (CompoundStmt []))
          <|> try (IfElse <$> (token "if" *> parend expression) <*> compoundStatement <*> (token "else" *> compoundStatement))
          <|> (statement' <* token ";")
          where statement' =  try (DeclareAssign <$> ty <*> identifier <*> (token "=" *> expression))
                          <|> try (Declare <$> ty <*> identifier) -- TODO handle array declarations
                          <|> try (Return <$> (token "return" *> expression))
                          <|> try (Expr <$> expression)
          -- loops todo

compoundStatement :: Parser Stmt
compoundStatement = CompoundStmt <$> braced (many statement)

-- top level

function :: Parser Function
function = undefined