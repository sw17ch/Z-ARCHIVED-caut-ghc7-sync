{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Support.Hs2010
  ( CauterizeSize(..)
  , CauterizeSerialize(..)

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
import Data.Bytes.Put
import Data.Bytes.Get
import qualified Data.ByteString as B
import qualified Data.Serialize.Put as S
import qualified Data.Serialize.Get as S

import Control.Monad

class CauterizeSize a where
  cautSize :: a -> Maybe Integer
  minSize :: a -> Integer
  minSize = maxSize

  maxSize :: a -> Integer
  maxSize = fromJust . cautSize

class CauterizeSerialize a where
  cautPut :: a -> S.PutM (Either String ())
  cautGet :: S.Get (Either String a)

cauterizePack :: (CauterizeSerialize a) => a -> Either String B.ByteString
cauterizePack a = case S.runPutM $ cautPut a of
                    (Left e, _) -> Left e
                    (Right _, v) -> Right v
                    
cauterizeUnpack :: (CauterizeSerialize a) => B.ByteString -> Maybe a
cauterizeUnpack b = case S.runGet b $ cautGet of
                      (Left e, _) -> Left e
                      (Right _, v) -> Right v

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

instance CauterizeSerialize U8 where
  cautPut x = liftM Just (putWord8 x)
