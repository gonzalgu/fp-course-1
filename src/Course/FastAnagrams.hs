{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str fp = filterLines <$> readFile fp
 where
    mkSet = S.fromList . hlist
    filterLines txt =
      let perms = mkSet (permutations str) -- S.fromList $ hlist (permutations str)
          wordsInDict = mkSet (lines txt)
          common = S.intersection perms wordsInDict
      in listh $ S.elems common
       

  {-
  
fastAnagrams str fp =
  readFile fp >>= \txt ->
                    let perms = S.fromList $ hlist $ permutations str
                        wordsInDict = S.fromList $ hlist $ lines txt
                        common = S.intersection perms wordsInDict
                        lst =  listh $ S.elems common
                    in pure lst

-}
 

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
