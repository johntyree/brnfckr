

module Brnfckr.Parse where


import Control.Applicative
import Data.Word


data BrainFuckError = UnbalancedBracket String
                    | ParseFailure String
                    | InsufficientInput
  deriving (Show, Eq)

data Term = ValIncr Word8
          | ValDecr Word8
          | ValSet  Word8
          | PtrIncr Int
          | PtrDecr Int
          | ScanIncr
          | ScanDecr
          | Loop [Term]
          | ValInput
          | ValOutput
  deriving (Eq, Show)

data Parser a = Parser { runParser :: String -> Either BrainFuckError (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser ((fmap . fmap . fmap) f p)

instance Applicative Parser where
  pure a = Parser (\xl -> Right (xl, a))
  Parser func <*> Parser p = Parser $ \xl ->
    case func xl of
      Right (xl', f) -> runParser (fmap f (Parser p)) xl'
      Left e -> Left e

instance Alternative Parser where
  empty = Parser (const $ Left (ParseFailure "empty"))
  Parser p <|> Parser q = Parser $ \xl ->
    case p xl of
      Right a -> Right a
      Left s -> case q xl of
        Right a -> Right a
        _ -> Left s

  some pp@(Parser p) = Parser some'
    where
      some' xl =
        case p xl of
          Left e -> Left e
          Right (xl', t) ->
            case runParser (many pp) xl' of
              Left _ -> Right (xl, [])
              Right (xl'', rest) -> Right (xl'', t:rest)

  many (Parser p) = Parser many'
    where
      many' xl =
        case p xl of
          Left _ -> Right (xl, [])
          Right (xl', t) ->
            case many' xl' of
              Left _ -> Right (xl, [])
              Right (xl'', rest) -> Right (xl'', t:rest)

instance Monad Parser where
  return = pure
  fail   s = Parser (const $ Left (ParseFailure s))
  Parser m >>= k = Parser $ \xl ->
    case m xl of
      Left e -> Left e
      Right (xl', a) ->
        let Parser n = k a
        in n xl'

parseBrainFuck :: String -> Either BrainFuckError [Term]
parseBrainFuck raw_source = case runParser (some parseTerm) source of
  Right ("", terms) -> Right terms
  Right (leftover, _) ->
    if leftover /= raw_source
    then parseBrainFuck leftover
    else Left $ ParseFailure leftover
  Left e -> Left e
  where
    source = filter (`elem` legalChars) raw_source
    legalChars = "+-><[],."

parseTerm :: Parser Term
parseTerm = Parser char'
  where char' [] = Left $ ParseFailure "Unexpected EOF"
        char' (x:xs) = case x of
          '+' -> Right (xs, ValIncr 1)
          '-' -> Right (xs, ValDecr 1)
          '<' -> Right (xs, PtrDecr 1)
          '>' -> Right (xs, PtrIncr 1)
          ',' -> Right (xs, ValInput)
          '.' -> Right (xs, ValOutput)
          '[' ->
            case runParser (parseBracket '[' ']') (x:xs) of
              Right (after, "") -> Right (after, Loop [])
              Right (after, inside) -> case runParser (some parseTerm) inside of
                Right (_, terms) -> Right (after, Loop terms)
                Left e -> Left e
              Left e -> Left e
          ']' -> Left $ UnbalancedBracket (x:xs)
          _ -> Left $ ParseFailure [x]

parseBracket :: Char -> Char -> Parser String
parseBracket open close = Parser go
  where
    go xl | null xl = Left $ ParseFailure "Not bracketed"
          | head xl == open = brkt (1 :: Int) "" (tail xl)
          | head xl == close = Left $ UnbalancedBracket xl
          | otherwise = Left $ ParseFailure "Not bracketed"
    brkt _ inner [] = Left $ UnbalancedBracket (open:reverse inner)
    brkt i inner (x:xs)
      | x == open = brkt (i+1) (x:inner) xs
      | i == 0 = Left $ ParseFailure ("Not a loop: " ++ (x:xs))
      | x == close = case i of
          1 -> Right (xs, reverse inner)
          0 -> Left $ UnbalancedBracket (reverse inner ++ (x:xs))
          _ -> brkt (i-1) (x:inner) xs
      | otherwise = brkt i (x:inner) xs


