module Parser
        ( BrainfuckSource (..)
        , BrainfuckCommand (..)
        , parse) where

import Control.Monad (foldM)
import Data.Maybe (isJust, fromJust)

-- | A single command in a Brainfuck program.
data BrainfuckCommand
  = MoveRight
  | MoveLeft
  | Increment
  | Decrement
  | Loop [BrainfuckCommand]
  | Input
  | Output
  deriving Eq

instance Show BrainfuckCommand where
  show MoveRight = ">"
  show MoveLeft  = "<"
  show Increment = "+"
  show Decrement = "-"
  show (Loop xs) = (filter (/=',') . show) xs
  show Input     = ","
  show Output    = "."

-- | Source code of a Brainfuck program.
data BrainfuckSource = BrainfuckSource [BrainfuckCommand]

instance Show BrainfuckSource where
  show (BrainfuckSource cs) = concatMap show cs

-- | Given a string, if it represents a valid Brainfuck program
-- parse it into the internal representation.
parse :: String -> Maybe BrainfuckSource
parse s = checkSyntax s >>= (Just . BrainfuckSource . parseString)

-- | Parse a valid string representation of a Brainfuck program.
parseString :: String -> [BrainfuckCommand]
parseString []       = []
parseString ('[':xs) = let (l, r) = splitAt (findMatchingClose xs 0) xs
                         in Loop (parseString l) : parseString (tail r)
parseString (x:xs)   = case parseCommand x of
  Just c  -> c : parseString xs
  Nothing -> parseString xs

-- | Parse a single Brainfuck command.
parseCommand :: Char -> Maybe BrainfuckCommand
parseCommand c = case c of
  '>' -> Just MoveRight
  '<' -> Just MoveLeft
  '+' -> Just Increment
  '-' -> Just Decrement
  ',' -> Just Input
  '.' -> Just Output
  _   -> Nothing

-- | Find the position of the matching closing bracket.
findMatchingClose :: String -> Int -> Int
findMatchingClose (']':_)  0 = 0
findMatchingClose (']':xs) n = 1 + (findMatchingClose xs (n-1))
findMatchingClose ('[':xs) n = 1 + (findMatchingClose xs (n+1))
findMatchingClose ~(_:xs)  n = 1 + (findMatchingClose xs n) -- It never receives an empty string
                                                            -- because that would not be a valid loop.

-- | Decide if the received string is a valid program.
-- The only possible syntax error (if we take all non-valid commands as comments)
-- is to have mismatching parenthesis.
checkSyntax :: String -> Maybe String
checkSyntax bfs
  | isValidSyntax bfs = Just bfs
  | otherwise         = Nothing

-- | Check if all parenthesis match.
isValidSyntax :: String -> Bool
isValidSyntax xs = (\x -> isJust x && (fromJust x) == 0)
  $ foldM f (0 :: Integer) xs
  where f ctr command
          | ctr < 0   = Nothing
          | otherwise = case command of
              '[' -> Just (ctr + 1)
              ']' -> Just (ctr - 1)
              _   -> Just ctr
