{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Support.Hs2010
  ( CauterizeSize(..)
  , CauterizeTagged(..)
  , CauterizeFlags(..)

  , U8
  , U16
  , U32
  , U64
  , S8
  , S16
  , S32
  , S64
  , Ieee754s
  , Ieee754d
  , Bool
  ) where

import Data.Word
import Data.Int
import Data.Maybe

class CauterizeSize a where
  cautSize :: a -> Maybe Integer
  minSize :: a -> Integer
  minSize = maxSize

  maxSize :: a -> Integer
  maxSize = fromJust . cautSize

class CauterizeTagged a where
  cautTag :: a -> Integer
  cautTagSize :: a -> Integer

class CauterizeFlags a where
  cautFlags :: a -> Integer
  cautFlagsSize :: a -> Integer


type U8 = Word8
type U16 = Word16
type U32 = Word32
type U64 = Word64
type S8 = Int8
type S16 = Int16
type S32 = Int32
type S64 = Int64
type Ieee754s = Float
type Ieee754d = Double
-- Just use Haskell's Bool for the built-in bool.

justConst :: a -> b -> Maybe a
justConst a _ = Just a

instance CauterizeSize U8 where
  cautSize = justConst 1

instance CauterizeSize U16 where
  cautSize = justConst 2

instance CauterizeSize U32 where
  cautSize = justConst 4

instance CauterizeSize U64 where
  cautSize = justConst 8

instance CauterizeSize S8 where
  cautSize = justConst 1

instance CauterizeSize S16 where
  cautSize = justConst 2

instance CauterizeSize S32 where
  cautSize = justConst 4

instance CauterizeSize S64 where
  cautSize = justConst 8

instance CauterizeSize Ieee754s where
  cautSize = justConst 4

instance CauterizeSize Ieee754d where
  cautSize = justConst 8

instance CauterizeSize Bool where
  cautSize = justConst 1
