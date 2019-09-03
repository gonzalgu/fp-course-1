{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Monad
import Course.Applicative

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams str fp = readFile fp >>= \txt -> 
                    pure $ intersectBy equalIgnoringCase (permutations str) (lines txt)

                    


  {-
  let perms = permutations str
  in readFile fp >>= \txt ->
       let fileLines = lines txt
       in pure $ intersectBy equalIgnoringCase perms fileLines
    -}   
{-                       
  do{
    txt <- readFile fp;
    let fileLines = lines txt;
    return $ intersectBy equalIgnoringCase perms fileLines    
}
-}

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase s1 s2 =
  (length s1 == length s2) && (and $ zipWith (\c1 c2 -> toLower c1 == toLower c2) s1 s2)
  
