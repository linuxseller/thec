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

parseFileContent :: Parser [AST]
parseFileContent = many $ consumeWS *> parser
  where parsers = [pAstInt
                  , pAstType
                  , pAstMain
                  , pAstParen
                  , pAstReturn
                  , pAstSemicolon
                  , pAstPrintf
                  , pAstString]
        parser = foldl1 (<|>) parsers

-- Parsing of AST

pAstInt :: Parser AST
pAstInt = (\dgs -> AstNum $ read dgs) <$> notNull (spanParser isDigit)

pAstString = (\x -> AstString x) <$> parseStringLiteral

allowedTypes = ["int"]

pAstType :: Parser AST
pAstType = (\t -> AstType t) <$> (foldl1 (<|>) $ map parseWord allowedTypes)

pAstMain :: Parser AST
pAstMain = AstMain <$ parseWord "main"

pAstPrintf :: Parser AST
pAstPrintf = AstPrintf <$ parseWord "printf"

pAstParen :: Parser AST
pAstParen = (\t -> AstParen t) <$> (foldl1 (<|>) $ map parseChar "(){}[]")

pAstReturn :: Parser AST
pAstReturn = AstReturn <$ parseWord "return"

pAstSemicolon :: Parser AST
pAstSemicolon = AstSemicolon <$ parseWord ";"
