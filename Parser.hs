module Parser where
  import Control.Applicative

  newtype Parser t = Parser { runParser :: String -> Maybe (t, String) }
  
  instance Functor Parser where
    fmap mapper (Parser f1) = Parser f2
      where
        f2 inputString = f1 inputString >>= \(x,xs) -> Just (mapper x, xs)
  
  pureParser :: a -> Parser a
  pureParser val = Parser (\x -> Just (val, x))
  
  parserApply :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  parserApply mapper (Parser f1) (Parser f2) = Parser f3
    where
      f3 inputString = do
        (val1, rest1) <- f1 inputString
        (val2, rest2) <- f2 rest1
        pure (mapper val1 val2, rest2)
  
  instance Applicative Parser where
    pure = pureParser
    (<*>) = parserApply id

  instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) (Parser f1) (Parser f2) = Parser f3 where f3 x = f1 x <|> f2 x
