module Cauterize.Support.Hs2010 where

import Data.Word
import Data.Int

class CauterizeSize a where
  cautSize :: a -> Integer
  minSize :: a -> Integer
  maxSize :: a -> Integer

class CauterizeTagged a where
  cautTag :: a -> Integer
  cautTagSize :: a -> Integer

class CauterizeFlags a where
  cautFlags :: a -> Integer
  cautFlagsSize :: a -> Integer
