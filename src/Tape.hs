module Tape where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N

-- | A tape is a Zipper data-structure for lists.
-- This mimics a doubly-linked list.
data Tape a = Tape [a] a [a]

instance Functor Tape where
  fmap f (Tape xs p ys) = Tape (fmap f xs) (f p) (fmap f ys)

instance Show a => Show (Tape a) where
  show (Tape xs p ys) = show (xs ++ (p:(take 20 ys)))

-- | Create a tape from a given list.
-- The list cannot be empty, because we need a pivot element.
fromList :: N.NonEmpty a -> Tape a
fromList (x :| xs) = Tape [] x xs

-- | Move the pointer to the right of the current position.
moveRight :: Tape a -> Maybe (Tape a)
moveRight (Tape xs p (y:ys)) = Just $ Tape (p:xs) y ys
moveRight _                  = Nothing

-- | Move the pointer to the left of the current position.
moveLeft :: Tape a -> Maybe (Tape a)
moveLeft (Tape (x:xs) p ys) = Just $ Tape xs x (p:ys)
moveLeft _                  = Nothing

-- | Tell if the tape is currently positioned at its first element.
start :: Tape a -> Bool
start (Tape [] _ _) = True
start _             = False

-- | Tell if the tape is currently positioned at its last element.
end :: Tape a -> Bool
end (Tape _ _ []) = True
end _             = False

-- | Use a function to update the element in the current position.
update :: (a -> a) -> Tape a -> Tape a
update f (Tape xs p ys) = Tape xs (f p) ys

-- | Write a value to the current position.
put :: a -> Tape a -> Tape a
put x = update (const x)

-- | Get the value in the current position.
get :: Tape a -> a
get (Tape _ p _) = p
