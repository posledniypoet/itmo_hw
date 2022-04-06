{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  ( parse
  )
where

import Control.Monad.Combinators.Expr
import Data.Char (isSpace)
import Data.Sequence as S hiding (empty)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import HW3.Base
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

rational :: Parser HiValue
rational = HiValueNumber . toRational <$> lexeme (L.signed sc L.scientific)

bool :: Parser HiValue
bool =
  HiValueBool
    <$> ( True <$ symbol "true"
            <|> False <$ symbol "false"
        )

nullParse :: Parser HiValue
nullParse = HiValueNull <$ symbol "null"

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

parseString :: Parser HiValue
parseString = lexeme $ HiValueString <$> (pack <$> stringLiteral)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parensSquare :: Parser a -> Parser a
parensSquare = between (symbol "[") (symbol "]")

list :: Parser HiExpr
list = do
  e <- HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> parensSquare parseArgs
  parseApply e

parseHiExpr :: Parser HiExpr
parseHiExpr = do
  fun <- parseHiValue
  parseApply fun

parseApply :: HiExpr -> Parser HiExpr
parseApply e =
  do
    a <- HiExprApply e <$> parseArgs
    parseApply a
    <|> return e

parseHiValue :: Parser HiExpr
parseHiValue = pHiFun <|> (HiExprValue <$> (rational <|> bool <|> nullParse <|> parseString)) <|> parens pExpr <|> list

parseArgs :: Parser [HiExpr]
parseArgs = parens $ pExpr `sepBy` symbol ","

pHiFun :: Parser HiExpr
pHiFun = HiExprValue . HiValueFunction <$> lexeme pairsHiFun

infixLeft :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixLeft name f = InfixL (f <$ symbol name)

infixNon :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixNon name f = InfixN (f <$ symbol name)

infixRight :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
infixRight name f = InfixR (f <$ symbol name)

op :: String -> Parser String
op n = (lexeme . try) (string n <* notFollowedBy "=")

opTable :: [[Operator Parser HiExpr]]
opTable =
  [ [ InfixL ((\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) [h1, h2]) <$ (lexeme . try) (string "/" <* notFollowedBy (string "="))),
      infixLeft "*" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunMul)) [h1, h2])
    ],
    [ infixLeft "+" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [h1, h2]),
      infixLeft "-" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunSub)) [h1, h2])
    ],
    [ infixNon "<" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunLessThan)) [h1, h2]),
      infixNon ">" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunGreaterThan)) [h1, h2]),
      infixNon "<=" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunNotGreaterThan)) [h1, h2]),
      infixNon ">=" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunNotLessThan)) [h1, h2]),
      infixNon "==" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunEquals)) [h1, h2]),
      infixNon "/=" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunNotEquals)) [h1, h2])
    ],
    [ infixRight "&&" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunAnd)) [h1, h2])
    ],
    [ infixRight "||" (\h1 h2 -> HiExprApply (HiExprValue (HiValueFunction HiFunOr)) [h1, h2])
    ]
  ]

pairsHiFun :: Parser HiFun
pairsHiFun =
  choice
    [ HiFunDiv <$ string "div",
      HiFunMul <$ string "mul",
      HiFunAdd <$ string "add",
      HiFunSub <$ string "sub",
      HiFunNot <$ string "not",
      HiFunAnd <$ string "and",
      HiFunOr <$ string "or",
      HiFunLessThan <$ string "less-than",
      HiFunGreaterThan <$ string "greater-than",
      HiFunEquals <$ string "equals",
      HiFunNotLessThan <$ string "not-less-than",
      HiFunNotGreaterThan <$ string "not-greater-than",
      HiFunNotEquals <$ string "not-equals",
      HiFunIf <$ string "if",
      HiFunLength <$ string "length",
      HiFunToUpper <$ string "to-upper",
      HiFunToLower <$ string "to-lower",
      HiFunTrim <$ string "trim",
      HiFunReverse <$ string "reverse",
      HiFunList <$ string "list",
      HiFunRange <$ string "range",
      HiFunFold <$ string "fold"
    ]

pExpr :: Parser HiExpr
pExpr = makeExprParser parseHiExpr opTable

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser pExpr (unpack "") . dropWhile isSpace

