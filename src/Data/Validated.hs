module Data.Validated where

import Control.Applicative

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

instance Applicative Validated where
  (Valid f)       <*> v                = fmap f v
  (Doubtful f ms) <*> (Valid v)        = Doubtful (f v) ms
  (Doubtful f ms) <*> (Doubtful v ms2) = Doubtful (f v) (ms2 ++ ms)
  v               <*> v2               = Invalid $ (messages v2) ++ (messages v)

  pure v = Valid v

