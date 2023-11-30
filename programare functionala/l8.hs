class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value

  keys c  = [ fst x|x <- (toList c)]
  values c = [snd x | x<- (toList c)]
  fromList [] =empty
  fromList ((k, v):kvs) = insert k v (fromList kvs)
--1
-- keys = map fst . toList
-- values = map snd . toList

-- fromList [] = empty
-- fromList ((k, v):kvs) = insert k v (fromList kvs)

--2

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

-- instance Collection PairList where
--     empty = PairList []
--     singleton k v = PairList [(k,v)] 
--     insert k v (PairList pairs) = PairList $ (k, v) : filter (\(key, _) -> key /= k) pairs

--     delete k (PairList pairs) = PairList $ filter (\(key, _) -> key /= k) pairs
--     keys (PairList pairs) = map fst pairs
--     values (PairList pairs) = map snd pairs
--     toList = getPairList
--     fromList = PairList

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k,v)]
    insert k v (PairList [])= singleton k v
    insert k v (PairList((cheie, valoare):t))
        | cheie==k = PairList ((cheie, v): t)
        | otherwise = PairList ((cheie, valoare): (getPairList (insert k v (PairList t))))
    clookup k  (PairList []) = Nothing
    clookup k  (PairList((cheie, valoare):t))
        | k==cheie = Just valoare
        | otherwise = clookup k (PairList t)

    delete k (PairList []) = empty
    delete k (PairList ((cheie, valoare): t))
        | cheie== k = (PairList t)
        | otherwise = PairList ((k,valoare): (getPairList (delete cheie (PairList t))))
    keys (PairList pairs) = map fst pairs
    values (PairList pairs) = map snd pairs
    toList = getPairList
    fromList = PairList

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty =Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert x v Empty = BNode Empty x (Just v) Empty
    insert x v (BNode left k value right ) =
        if x<=k
            then insert x v left
         else insert x v right
    delete _ Empty = Empty
    delete x (BNode left k value right )
        | (k==x) = (BNode left k Nothing right )
        | (x<k) = delete x  left
        | otherwise = delete x  right

    clookup  x  Empty = Nothing
    clookup x (BNode left k value right )
        | x==k =value
        | x<k = clookup x left
        | otherwise = clookup x right
    keys Empty =[]
    keys (BNode left k value right ) = (keys left) ++[k] ++(keys right)

    values Empty = []
    values (BNode left k Nothing right )  = (values left) ++(values right)
    values (BNode left k (Just v) right )  = (values left) ++ [v]++(values right)

    toList Empty =[]
    toList (BNode left key Nothing right )=(toList left ) ++(toList right)
    toList (BNode left key (Just val) right) =(toList left ) ++[(key, val)] ++(toList right)

    fromList lista = foldr (\(x,y) acc ->insert x y acc) Empty lista




data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
        toArb :: a -> Arb
        fromArb :: Arb -> a

instance Collection Arb where
    Show 

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

