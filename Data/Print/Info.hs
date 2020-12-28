-- |
-- Module      :  Data.Print.Info
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Can be used to coordinate the printing output.

module Data.Print.Info where

-- | Is used to define what information is printed. The greater values correspond to more verbose output.
data Info = A | B | C | D | E | F | G | H deriving (Eq, Ord)

-- | The same as 'Info' but is used for cases of printing to the file specified as a 'String' parameter with the corresponding first capital letter 
-- analogue of 'Info' data type.
data InfoFile = Af String | Bf String | Cf String | Df String | Ef String | Ff String | Gf String | Hf String deriving (Eq, Ord)

fromInfoFile :: InfoFile -> String
fromInfoFile (Af xs) = xs
fromInfoFile (Bf xs) = xs
fromInfoFile (Cf xs) = xs
fromInfoFile (Df xs) = xs
fromInfoFile (Ef xs) = xs
fromInfoFile (Ff xs) = xs
fromInfoFile (Gf xs) = xs
fromInfoFile (Hf xs) = xs

data InfoG a b = I1 a | I2 b deriving Eq

-- | Type synonym used to coordinate the printing output in general case. 
type Info2 = InfoG Info InfoFile

isI1 :: InfoG a b -> Bool
isI1 (I1 _) = True
isI1 _ = False

isI2 :: InfoG a b -> Bool
isI2 (I2 _) = True
isI2 _ = False

