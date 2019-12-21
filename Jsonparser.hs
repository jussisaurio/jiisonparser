{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Jsonparser (jiisonParse) where
  import Data.Functor
  import Data.Either
  import qualified Data.Map as M
  import Control.Applicative
  import Data.Char (isDigit)
  import Text.Read (readMaybe)
  import Parser
  import Helpers

  data JSON = S String | N Float | B Bool | Null | O (M.Map String JSON) | A [JSON] deriving (Eq, Show)

  charWithEscape :: Parser Char
  charWithEscape = Parser (\case
                    [] -> Nothing
                    [c] -> Just (c, [])
                    '\\':c:cs -> Just (c, cs)
                    '"':cs -> Nothing
                    c:cs -> Just (c, cs))

  key :: Parser String
  key = trim (quotes (oneOrMore charWithEscape)) <* char ':'

  number :: Parser JSON
  number = Parser readFloat where
            readFloat :: String -> Maybe (JSON, String)
            readFloat str = (, xs) . N <$> readMaybe numStr where (numStr, xs) = span (\c -> isDigit c || c `elem` ['-','.']) str

  nulltype :: Parser JSON
  nulltype = Parser (\str -> if take 4 str == "null" then Just (Null, drop 4 str) else Nothing)

  boolean :: Parser JSON
  boolean = Parser func where
              func i
                | take 4 i == "true" = Just (B True, drop 4 i)
                | take 5 i == "false" = Just (B False, drop 5 i)
                | otherwise = Nothing

  string :: Parser JSON
  string = S <$> quotes (zeroOrMore charWithEscape)

  listOf :: Parser a -> Parser [a]
  listOf parser = ((++) <$> zeroOrMore (parser <* char ',') <*> ((:[]) <$> parser)) <|> pure []

  mapOf :: Ord a => Parser (a, b) -> Parser (M.Map a b)
  mapOf parser = foldr (uncurry M.insert) M.empty <$> listOf parser

  primitive = trim number <|> trim boolean <|> trim string <|> trim nulltype

  entry = (,) <$> key <*> json

  array = A <$> trim (char '[' *> listOf json <* char ']')

  object = O <$> trim (char '{' *> mapOf entry <* char '}')

  json = primitive <|> array <|> object

  newtype ParseError = ParseError String deriving (Eq, Show)

  jiisonParse :: String -> Either ParseError JSON
  jiisonParse str = case runParser json str of
                      Nothing -> Left $ ParseError "Invalid JSON"
                      Just (parsedJson, remainder) -> if remainder /= "" then Left $ ParseError "Invalid JSON" else Right parsedJson
