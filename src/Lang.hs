module Lang where

import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec

langDef :: LanguageDef st
langDef = LanguageDef
            { commentStart   = "/*"
            , commentEnd     = "*/"
            , commentLine    = "//"
            , nestedComments = False
            , identStart     = letter <|> char '_'
            , identLetter    = alphaNum <|> oneOf "_"
            , opStart        = oneOf "+-*%/|<>!=~&*"
            , opLetter       = oneOf "+-*%/|<>!=~&*"
            , reservedOpNames= []
            , reservedNames  = ["int", "char"]
            , caseSensitive  = True
            }
