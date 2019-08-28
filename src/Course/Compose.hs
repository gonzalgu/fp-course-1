{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
{- 
 h :: a -> b
 x :: f (g a)
 <$> :: (a -> b) -> Compose f g a -> Compose f g b 
-}      
  h <$> (Compose x) = Compose $ ((h <$>) <$> x)
    

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure x = Compose $ pure (pure x)
    
-- Implement the (<*>) function for an Applicative instance for Compose
{-
x :: f (g a)
h :: f(g(a -> b))
<$> :: (a -> b) -> f a -> f b
<*> :: f (a -> b) -> f a -> f b
ret :: f(g b)
-}
  (Compose h) <*> (Compose x) = Compose $ 
    (\gab ga -> gab <*> ga) <$> h <*> x
    

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) = --impossible
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
