{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Support.Hs2010
  ( CauterizeSize(..)
  , CauterizeSerialize(..)
  , cauterizePack
  , cauterizeUnpack
  
  , putIfJust
  , boolsToBits

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
import Data.Bits
-- import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bytes.Serial
import qualified Data.ByteString as B
import qualified Data.Serialize.Put as S
import qualified Data.Serialize.Get as S

import Control.Monad.Trans
import Control.Monad.Trans.Except

class CauterizeSize a where
  cautSize :: a -> Maybe Integer
  minSize :: a -> Integer
  minSize = maxSize

  maxSize :: a -> Integer
  maxSize = fromJust . cautSize

class CauterizeSerialize a where
  cautPut :: a -> ExceptT String S.PutM ()
  cautGet :: ExceptT String S.Get a

cauterizePack :: (CauterizeSerialize a) => a -> Either String B.ByteString
cauterizePack a = case S.runPutM . runExceptT $ cautPut a of
                    (Left e, _) -> Left e
                    (Right _, v) -> Right v
                    
cauterizeUnpack :: (CauterizeSerialize a) => B.ByteString -> Either String a
cauterizeUnpack b = let a = runExceptT cautGet
                    in case S.runGet a b of
                          Left l -> Left l
                          Right (Left l) -> Left l
                          Right (Right r) -> Right r

putIfJust :: CauterizeSerialize a => Maybe a -> ExceptT String S.PutM ()
putIfJust (Just v) = cautPut v
putIfJust Nothing = lift $ return ()

boolsToBits :: (Num a, Bits a) => [Bool] -> a
boolsToBits bools = go 0 $ zip bools [0..]
  where
    go a [] = a
    go a ((bool, idx):bs) = let a' = if bool then a `setBit` idx else a
                            in go a' bs

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

instance CauterizeSize () where
  cautSize = justConst 0

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

cput :: SerialEndian a => a -> ExceptT String S.PutM ()
cput x = lift $ serializeLE x

cget :: SerialEndian a => ExceptT String S.Get a
cget = lift deserializeLE

instance CauterizeSerialize U8 where
  cautPut x = lift $ serialize x
  cautGet = lift deserialize

instance CauterizeSerialize U16 where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize U32 where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize U64 where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize S8 where
  cautPut x = lift $ serialize x
  cautGet = lift deserialize

instance CauterizeSerialize S16 where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize S32 where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize S64 where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize Ieee754s where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize Ieee754d where
  cautPut = cput
  cautGet = cget

instance CauterizeSerialize Bool where
  cautPut x = lift $ serialize ((fromIntegral . fromEnum) x :: Word8)
  cautGet = do
    w <- deserialize :: MonadGet m => m Word8
    case w of
      0 -> lift $ return False
      1 -> lift $ return True
      _ -> throwE $ "Invalid enumeration value: " ++ show w
