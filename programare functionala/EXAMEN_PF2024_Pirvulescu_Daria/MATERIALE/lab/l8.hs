
class Collection c  where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert :: Ord key => key -> value -> c key value -> c key value
    clookup :: Ord key => key -> c key value -> Maybe value
    delete :: Ord key => key -> c key value -> c key value
    keys :: c key value -> [key]
    values :: c key value -> [value]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value
    
    keys c = [fst x | x <- (toList c)]
    values c = [snd x | x <- (toList c)]
    fromList [] = empty
    fromList ((k, v):t) = insert k v (fromList t)
    

newtype PairList k v = PairList { getPairList :: [(k, v)] } deriving Show

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k,v)]
    insert k v (PairList []) = singleton k v
    insert k v (PairList ((cheie, valoare):t))
        | cheie == k = PairList ((cheie, v) : t)
        | otherwise = PairList ((cheie, valoare) : (getPairList (insert k v (PairList t))))

    clookup cheie (PairList []) = Nothing
    clookup cheie (PairList ((k,val) : t))
        | cheie == k = Just val
        | otherwise = clookup cheie (PairList t)
    
    delete cheie (PairList []) = empty
    delete cheie (PairList ((k, val) : t))
        | k == cheie = (PairList t)
        | otherwise = PairList( (k,val) : (getPairList (delete cheie (PairList t)))) 

    toList l = getPairList l

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
    deriving Show

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty

    insert k v (Empty) = singleton k v
    insert k v (BNode left cheie valoare right)
        | cheie == k = BNode left cheie (Just v) right
        | k < cheie = BNode (insert k v left) cheie valoare right
        | otherwise = BNode left cheie valoare (insert k v right)

    clookup k (Empty) = Nothing
    clookup k (BNode left cheie valoare right)
        | cheie == k = valoare
        | k < cheie = clookup k left
        | otherwise = clookup k right
    
    delete k (Empty) = Empty
    delete k (BNode left cheie val right)
        | k == cheie = BNode left cheie Nothing right
        | k < cheie = BNode (delete k left) cheie val right
        | otherwise = BNode left cheie val (delete k right)

    toList (Empty) = []
    toList (BNode left cheie (Just valoare) right) = (toList left) ++ [(cheie,valoare)] ++ (toList right)
    toList (BNode left cheie Nothing right) = (toList left) ++ (toList right)
        
arbore = BNode Empty "a" (Just 1) (BNode Empty "b" (Just 2) (Empty))

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where 
    toArb :: a -> Arb 
    fromArb :: Arb -> a
-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793
