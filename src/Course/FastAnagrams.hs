{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str fp = do{
  let perms = S.fromList $ NoCaseString <$> permutations str
  lines <- S.fromList (NoCaseString <$> (lines $ readFile fp))
  pure $ S.intersection perms lines
}
  --error "todo: Course.FastAnagrams#fastAnagrams"

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
