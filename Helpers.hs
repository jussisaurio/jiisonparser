module Helpers where
  import Parser
  import Control.Applicative
  import Data.Char (isSpace)
  satisfy :: (Char-> Bool) -> Parser Char
  satisfy predicate = Parser func
    where
      func [] = Nothing
      func (x:xs)
        | predicate x = Just (x, xs)
        | otherwise = Nothing
  
  char :: Char -> Parser Char
  char c = satisfy (== c)

  zeroOrMore :: Parser a -> Parser [a]
  zeroOrMore p = oneOrMore p <|> pure []
  
  oneOrMore :: Parser a -> Parser [a]
  oneOrMore p = (:) <$> p <*> zeroOrMore p
  
  spaces :: Parser String
  spaces = zeroOrMore (satisfy isSpace)

  trim :: Parser a -> Parser a
  trim parser = spaces *> parser <* spaces

  quotes :: Parser a -> Parser a
  quotes parser = char '"' *> parser <* char '"'
