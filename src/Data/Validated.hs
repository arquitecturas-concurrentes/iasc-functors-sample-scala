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
  (Doubtful f ms) <*> (Doubtful v ms2) = Doubtful (f v) (ms ++ ms2)
  (Doubtful _  _) <*> (Invalid    ms2) = Invalid ms2
  (Invalid ms)    <*> _                = Invalid ms

  pure v = Valid v

instance Monad Validated where
  (Valid v) >>= f      = f v
  (Invalid m) >>= _   = (Invalid m)
  (Doubtful v m) >>= f = case f v of
            Valid r -> Doubtful r m
            Doubtful r m2 -> Doubtful r (m ++ m2)
            Invalid m2    -> Invalid m2

  return = pure