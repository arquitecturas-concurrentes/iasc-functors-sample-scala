module Data.Validated where

data Validated a = Valid a
                 | Doubtful a [String]
                 | Invalid [String] deriving (Eq, Show)

messages (Valid _) = []
messages (Doubtful _ ms) = ms
messages (Invalid ms) = ms

instance Functor Validated where
  fmap f (Valid a) = Valid (f a)
  fmap f (Doubtful a ms) = Doubtful (f a) ms
  fmap _ (Invalid ms) = (Invalid ms)

