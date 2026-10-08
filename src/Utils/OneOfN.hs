{-# LANGUAGE DeriveDataTypeable #-}

module Utils.OneOfN where

import Data.Data (Data, Typeable)

-- | Holds one of 1 possible value
newtype OneOf1 a1
  = OneOf1 a1
  deriving (Eq, Show, Read, Data, Typeable)

-- | Holds one of 2 possible values
data OneOf2 a1 a2
  = OneOf2_1 a1
  | OneOf2_2 a2
  deriving (Eq, Show, Read, Data, Typeable)

-- | Holds one of 3 possible values
data OneOf3 a1 a2 a3
  = OneOf3_1 a1
  | OneOf3_2 a2
  | OneOf3_3 a3
  deriving (Eq, Show, Read, Data, Typeable)

-- | Holds one of 4 possible values
data OneOf4 a1 a2 a3 a4
  = OneOf4_1 a1
  | OneOf4_2 a2
  | OneOf4_3 a3
  | OneOf4_4 a4
  deriving (Eq, Show, Read, Data, Typeable)

-- | Holds one of 5 possible values
data OneOf5 a1 a2 a3 a4 a5
  = OneOf5_1 a1
  | OneOf5_2 a2
  | OneOf5_3 a3
  | OneOf5_4 a4
  | OneOf5_5 a5
  deriving (Eq, Show, Read, Data, Typeable)
