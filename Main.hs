type Bit = Int
type Sequence a = [a]
type Set a = [a]
type Bitset = [Int]

generateBitArray :: Eq a => Sequence a -> Set a -> [Bit]
generateBitArray seq set = map (boolToBit . (`elem` set)) seq

boolToBit :: Bool -> Bit
boolToBit bool = if bool then 1 else 0

isBitSubset :: Bitset -> Bitset -> (Bool, Bool)
isBitSubset b1 b2 = (foo b1 b2, foo b2 b1)
  where
    foo x y = and $ zipWith (\x y -> x == 0 || x == y) x y

isSubsetOf :: Eq a => Set a -> Set a -> Bool
isSubsetOf x y = 0 `notElem` generateBitArray x y