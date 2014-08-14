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
                    
cauterizeUnpack :: (CauterizeSerialize a) => B.ByteString -> Either String a
cauterizeUnpack b = let e = S.runGet cautGet b :: CauterizeSerialize a => Either String (Either String a)
                    in case e of
                          Left l -> Left l
                          Right (Left l) -> Left l
                          Right (Right r) -> Right r

putIfJust :: CauterizeSerialize a => Maybe a -> S.PutM (Either String ())
putIfJust (Just v) = cautPut v
putIfJust Nothing = return . Right $ ()

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

cput :: SerialEndian a => a -> S.PutM (Either String ())
cput x = liftM Right (serializeLE x)

cget :: SerialEndian a => S.Get (Either String a)
cget = liftM Right deserializeLE

instance CauterizeSerialize U8 where
  cautPut x = liftM Right $ serialize x
  cautGet = liftM Right deserialize

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
  cautPut x = liftM Right $ serialize x
  cautGet = liftM Right deserialize

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
  cautPut x = liftM Right $ serialize ((fromIntegral . fromEnum) x :: Word8)
  cautGet = do
    w <- deserialize :: MonadGet m => m Word8
    case w of
      0 -> return (Right False)
      1 -> return (Right True)
      _ -> return (Left $ "Invalid enumeration value: " ++ show w)
