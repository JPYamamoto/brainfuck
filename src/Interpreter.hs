module Interpreter
        ( run
        , step
        , stepAll
        , emptyMemory
        , dumpMemory
        ) where

import Parser
import Tape
import Data.Char (chr)
import Data.Word
import Text.Read (readMaybe)

-- Type Definitions
type Cell = Word8       -- Change to Integer for arbitrary size values.
type Memory = Tape Cell

-- | Create a tape of memory with all values set to 0.
emptyMemory :: Memory
emptyMemory = Tape [] 0 (repeat 0)

-- | Perform an operation, according to the received command,
-- and the state of the memory.
execute :: Memory -> BrainfuckCommand -> IO (Maybe Memory)
execute m MoveRight  = return $ moveRight m
execute m MoveLeft   = return $ moveLeft m
execute m Increment  = return $ Just (update (+1) m)
execute m Decrement  = return $ Just (update (subtract 1) m)
execute m l@(Loop _) = executeLoop m l
execute m Input      = do
  input <- getLine
  case (readMaybe input :: Maybe Cell) of
    Just x  -> return $ Just (put x m)
    Nothing -> return Nothing
execute m Output     = do
  putStr $ formatOutput (get m)
  return (Just m)

-- | Handle the execution of loops and return the resulting
-- memory.
executeLoop :: Memory -> BrainfuckCommand -> IO (Maybe Memory)
executeLoop m ~(Loop cs)
  | (get m /= 0) = do
      newMem <- stepAll m (BrainfuckSource cs)
      case newMem of
        Just x  -> if (get x) /= 0 then executeLoop x (Loop cs) else return newMem
        Nothing -> return Nothing
  | otherwise = return (Just m)

-- | Go to the following instruction in the source code.
nextInstruction :: BrainfuckSource -> BrainfuckSource
nextInstruction (BrainfuckSource s) = (BrainfuckSource (tail s))

-- | Perform a single operation of the source code.
-- When this reads a loop command, the whole loop is executed.
step :: Memory -> BrainfuckSource -> IO (Maybe (Memory, BrainfuckSource))
step m s@(BrainfuckSource (c:_)) = do
  new_mem <- execute m c
  case new_mem of
    Just n  -> return $ Just (n, nextInstruction s)
    Nothing -> return Nothing
step _ _                          = return Nothing

-- | Perform all the operations until the program halts.
stepAll :: Memory -> BrainfuckSource -> IO (Maybe Memory)
stepAll m (BrainfuckSource []) = return (Just m)
stepAll m s                    = do
  stepResult <- step m s
  case stepResult of
    Just (m', s') -> stepAll m' s'
    Nothing       -> return Nothing

-- | Run a program received as a string.
run :: String -> IO ()
run s = case parse s of
  Just x  -> do
    result <- stepAll emptyMemory x
    case result of
      Just y -> dumpMemory y
      Nothing -> putStrLn "Program tried to access unavailable memory."
  Nothing -> do
    putStrLn "Invalid program."

-- | Print the current state of the memory.
dumpMemory :: Memory -> IO ()
dumpMemory (Tape xs p ys) = do
  putStrLn "\nMemory dump:"
  putStrLn $ formatSection (reverse xs) ++ " " ++ show p ++ " " ++ formatSection (take 20 ys)
  return ()
  where formatSection = (init . tail . show)

-- | Format a cell as an ASCII character when possible.
-- Otherwise, the cell just shows its value.
formatOutput :: Cell -> String
formatOutput x
  | x >= 0 && x <= 255 = [(chr . fromEnum) x]
  | otherwise          = show x
