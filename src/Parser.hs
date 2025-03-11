module Parser where
import Data.Parser

import Control.Applicative
import Data.Char
import Data.Tuple

parseWord :: String -> Parser String
parseWord str =
  (sequenceA $ map parseChar str)
    <* (Parser $ \(x:xs) -> if (not.isAlpha) x then Just (x:xs, "") else Nothing)

parseChar :: Char -> Parser Char
parseChar char =
  Parser $ \input -> case input of
    (x:xs) | x == char -> Just (xs, char)
    _ -> Nothing

consumeWS :: Parser String
consumeWS = spanParser isSpace

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser $ \input -> Just $ swap $ span f input

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
                (input', xs) <- p input
                if null xs then Nothing else Just (input', xs)

parseInt :: Parser String
parseInt = notNull $ spanParser isDigit

parseStringLiteral :: Parser String
parseStringLiteral = parseChar '"' *> spanParser (/='"') <* parseChar '"'

parseFileContent :: Parser [Token]
parseFileContent = many $ consumeWS *> parser
  where parsers = [pTokenInt
                  , pTokenType
                  , pTokenMain
                  , pTokenParen
                  , pTokenReturn
                  , pTokenSemicolon
                  , pTokenPrintf
                  , pTokenString
                  , pTokenComma]
        parser = foldl1 (<|>) parsers

-- Parsing of Token

pTokenInt :: Parser Token
pTokenInt = (\dgs -> TokenNum $ read dgs) <$> notNull (spanParser isDigit)

unescape :: String -> String
unescape [] = []
unescape [x] = [x]
unescape ('\\':'n':xs) = "\", 10, \"" <> (unescape xs)
unescape ('\\':'\\':xs) = '\\' : (unescape xs)
unescape ('\\':'"':xs) = '"' : (unescape xs) -- TODO: fix
unescape (x:xs) = x : (unescape xs)

pTokenString = (\x -> TokenString $ unescape x) <$> parseStringLiteral

allowedTypes = ["int"]

pTokenType :: Parser Token
pTokenType = (\t -> TokenType t) <$> (foldl1 (<|>) $ map parseWord allowedTypes)

pTokenMain :: Parser Token
pTokenMain = TokenMain <$ parseWord "main"

pTokenPrintf :: Parser Token
pTokenPrintf = TokenPrintf <$ parseWord "printf"

pTokenParen :: Parser Token
pTokenParen = (\t -> TokenParen t) <$> (foldl1 (<|>) $ map parseChar "(){}[]")

pTokenReturn :: Parser Token
pTokenReturn = TokenReturn <$ parseWord "return"

pTokenComma :: Parser Token
pTokenComma = TokenComma <$ parseWord "," -- non alnum fixme: ??

pTokenSemicolon :: Parser Token
pTokenSemicolon = TokenSemicolon <$ parseWord ";"
