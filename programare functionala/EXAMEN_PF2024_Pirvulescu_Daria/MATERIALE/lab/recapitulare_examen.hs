import Control.Arrow (ArrowChoice(left))
import Text.XHtml (base)
import Data.List (nub)
import Data.Maybe (fromJust)
import System.Win32 (xBUTTON1, COORD (xPos))
import Data.Set (cartesianProduct)

-- constt :: a -> b -> a
-- constt x y = x
-- replaceWithP = constt 'p'

-- class Collection c where
--   empty :: c key value
--   singleton :: key -> value -> c key value
--   insert:: Ord key => key -> value -> c key value -> c key value
--   clookup :: Ord key => key -> c key value -> Maybe value
--   delete :: Ord key => key -> c key value -> c key value
--   keys :: c key value -> [key]
--   values :: c key value -> [value]
--   toList :: c key value -> [(key, value)]
--   fromList :: Ord key => [(key,value)] -> c key value

--   keys c = [fst x| x<-(toList c)]
--   values c = [snd y | y <- (toList c)]
--   fromList ((key,value):t) = insert key value (fromList t)

-- newtype PairList k v
--   = PairList { getPairList :: [(k, v)] }

-- instance Collection PairList where
--     empty = PairList []
--     singleton a b = PairList [(a,b)]
--     insert key value (PairList[]) = singleton key value
--     insert key value (PairList((k,v):t))
--         |k==key= PairList ((k,value):t)
--         |otherwise = PairList ((k,v):getPairList (insert key value (PairList t)))
--     clookup k (PairList [])= Nothing
--     clookup k (PairList ((key,value):t))
--         |key==k = Just value
--         |otherwise = clookup k (PairList t)
--     delete k (PairList []) = empty
--     delete k (PairList((key,value):t))
--         |k==key = PairList t
--         |otherwise = PairList ((key,value):getPairList (delete k (PairList t)))
--     toList t= getPairList t


-- data SearchTree key value
--   = Empty
--   | BNode
--       (SearchTree key value) -- elemente cu cheia mai mica
--       key                    -- cheia elementului
--       (Maybe value)          -- valoarea elementului
--       (SearchTree key value) -- elemente cu cheia mai mare
--     deriving Show
-- instance Collection SearchTree where
--     empty = Empty
--     singleton k v = BNode empty k (Just v) empty
--     insert k v Empty = singleton k v
--     insert k v (BNode left key value right)
--         |k==key = BNode left key (Just v) right
--         |k<key = BNode (insert k v left) key value right
--         |otherwise = BNode left key value (insert k v right)
--     clookup k Empty = Nothing
--     clookup k (BNode left key value right)
--         |k==key =  value
--         |k<key = clookup k left
--         |otherwise = clookup k right
--     delete k Empty = empty
--     delete k (BNode left key value right)
--         |k==key = BNode left key Nothing right
--         |k<key = BNode (delete k left ) key value right
--         |otherwise = BNode left key value (delete k right)
--     toList Empty = []
--     toList (BNode left key Nothing right) = toList left ++ toList right
--     toList (BNode left key (Just v) right) =toList left ++[(key,v)]++ toList right

-- arbore = BNode Empty "a" (Just 1) (BNode Empty "b" (Just 2) Empty)

-- data Punct = Pt [Int]

-- -- instance Show Punct where
-- --   show (Pt []) = "()"
-- --   show (Pt (h:t)) = "(" ++ show(h) ++ (foldr (\x y -> ", " ++ show(x) ++ y) ")" t)

-- instance Show Punct where
--     show (Pt []) = ""
--     show (Pt (h: t)) = "(" ++ showsep h t ++ ")"
--         where showsep x [] = show x
--               showsep x (h : t) = show x ++ "," ++ showsep h t

-- data Arb = Vid | F Int | N Arb Arb
--           deriving Show

-- class ToFromArb a where
--             toArb :: a -> Arb
--             fromArb :: Arb -> a

-- instance ToFromArb Punct where
--     toArb (Pt []) = Vid
--     toArb (Pt (h:t)) = N (F h) (toArb (Pt t))

--     fromArb Vid = Pt []
--     fromArb (F x) = Pt [x]
--     fromArb (N left right) = Pt (list_left ++ list_right)
--                               where
--                                 Pt list_left = fromArb left
--                                 Pt list_right = fromArb right


-- -- Pt [1,2,3]
-- -- (1, 2, 3)

-- -- Pt []
-- -- ()

-- -- toArb (Pt [1,2,3])
-- -- N (F 1) (N (F 2) (N (F 3) Vid))
-- -- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
-- --  (1,2,3)
-- data Geo a = Square a | Rectangle a a | Circle a
--     deriving Show

-- class GeoOps g where
--   perimeter :: (Floating a) => g a -> a
--   area :: (Floating a) =>  g a -> a

-- instance GeoOps Geo where
--     perimeter :: Floating a => Geo a -> a
--     perimeter (Square l) = 4*l
--     perimeter (Rectangle a b) = 2*(a+b)
--     perimeter (Circle r) = 2*r*(pi^2)
--     area (Square l) = l*l
--     area (Rectangle a b) = a*b
--     area (Circle r) = pi*r*r

-- instance (Eq a, Floating a) => Eq (Geo a) where
--     geo_a == geo_b = perimeter geo_a == perimeter geo_b
-- -- ghci> pi
-- -- 3.141592653589793

-- eDiv x y = if y == 0 then Left " Division by 0! "
--            else Right ( div x y )
-- eF x = ( + ) <$> pure 4 <*> eDiv 10 x


-- newtype Writer log a = Writer { runWriter :: (a, log) }
--     deriving Show




-- instance Monad (Writer String) where
--     return va = Writer (va, "")
--     ma >>= f = let (va, log1) = runWriter ma
--                    (vb, log2) = runWriter (f va)
--                in Writer (vb, log1 ++ log2)

-- instance  Applicative (Writer String) where
--   pure = return
--   mf <*> ma = do
--     f <- mf
--     a <- ma
--     return (f a)

-- instance  Functor (Writer String) where
--   fmap f ma = pure f <*> ma

-- tell :: String -> Writer String ()
-- tell msg = Writer ((), msg)

-- logIncrement :: Int -> Writer String Int
-- logIncrement x = do
--     tell ("increment: " ++ show x ++ "--")
--     return (x + 1)

-- logIncrement2 :: Int -> Writer String Int
-- {-logIncrement2 x = do
--     y <- logIncrement x
--     logIncrement y
-- -}
-- logIncrement2 x = logIncrement x >>= \y -> logIncrement y



type Nume = String
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
-- 2. (P ∨ Q) ∧ (¬P ∧ ¬Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: ( Not (Var  "P") :&: Not ( Var "Q"))
-- 3. (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))
p3 :: Prop
p3 = (Var "P" :&:(Var "Q" :|: Var "R")) :&: (( Not (Var  "P") :|: Not ( Var "Q")):&:( Not (Var  "P") :|: Not ( Var "R")))


instance Show Prop where
  show (Var x)= x
  show ( x :&:  y)= "("++ show x ++ "&" ++ show y ++ ")"
  show ( x :|:  y)= "("++ show x ++ "|" ++ show y ++ ")"
  show ( x :->:  y)= "("++ show x ++ "->" ++ show y ++ ")"
  show ( x :<->: y)= "("++ show x ++ "<->" ++ show y ++ ")"
  show (Not x)= "~" ++ show x


test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval = undefined

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

variabile :: Prop -> [Nume]
variabile = undefined

test_variabile =
  variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs = undefined

test_envs =
    envs ["P", "Q"]
    ==
    [ [ ("P",False)
      , ("Q",False)
      ]
    , [ ("P",False)
      , ("Q",True)
      ]
    , [ ("P",True)
      , ("Q",False)
      ]
    , [ ("P",True)
      , ("Q",True)
      ]
    ]

satisfiabila :: Prop -> Bool
satisfiabila = undefined

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida = undefined

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True


echivalenta :: Prop -> Prop -> Bool
echivalenta = undefined

test_echivalenta1 =
  True
  ==
  (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))
test_echivalenta2 =
  False
  ==
  (Var "P") `echivalenta` (Var "Q")
test_echivalenta3 =
  True
  ==
  (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))

{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
newtype Identity a = Identity a
    deriving Show

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)


data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Constant a b = Constant b

instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)

data Two a b = Two a b

instance Functor (Two a ) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c)= Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d

instance Functor (Four a b c ) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b

instance Functor (Four'' b) where
    fmap f (Four'' a b c d)= Four'' a b c (f d)

data Quant a b = Finance | Desk a | Bloor b



data LiftItOut f a = LiftItOut (f a)

data Parappa f g a = DaWrappa (f a) (g a)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

data Notorious g o a t = Notorious (g o) (g a) (g t)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

data TalkToMe a = Halt | Print String a | Read (String -> a)



{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
-- data List a = Nil
--             | Cons a (List a)
--         deriving (Eq, Show)

-- instance Functor List where
--     fmap _ Nil= Nil
--     fmap f (Cons a b) = Cons (f a) (fmap f b)
-- instance Applicative List where
--     pure f = Cons f Nil
--     Nil <*> _ = Nil
--     _ <*> Nil = Nil
--     (Cons cap restul) <*> lista_de_obiecte = concatenare (fmap cap lista_de_obiecte) (restul<*>lista_de_obiecte)

-- concatenare:: List a-> List a -> List a
-- concatenare l1 Nil = l1
-- concatenare (Cons x l1) l2 = Cons x (concatenare l1 l2)

-- f = Cons (+1) (Cons (*2) Nil)
-- v = Cons 1 (Cons 2 Nil)
-- test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- data Cow = Cow {
--         name :: String
--         , age :: Int
--         , weight :: Int
--         } deriving (Eq, Show)

-- noEmpty :: String -> Maybe String
-- noEmpty "" = Nothing
-- noEmpty s = Just s 

-- noNegative :: Int -> Maybe Int
-- noNegative x 
--         |x<0 = Nothing
--         |otherwise = Just x

-- test21 = noEmpty "abc" == Just "abc"
-- test22 = noNegative (-5) == Nothing 
-- test23 = noNegative 5 == Just 5 

-- -- cowFromString :: String -> Int -> Int -> Maybe Cow
-- -- cowFromString n v g
-- --         |noEmpty n ==Nothing || noNegative v== Nothing || noNegative g == Nothing = Nothing
-- --         |otherwise = Just Cow {name = n, age = v, weight = g}

-- -- Cow :: Sting -> (Int -> Int -> Cow)
-- -- No empty :: String -> Maybe String
-- -- no negative :: Int -> Maybe Int
-- -- fmap :: (a->b) -> f a-> f b
-- -- <*>:: f (a->b) -> f a -> f b
-- -- Maybe (Int -> (Int -> Cow))
-- -- Maybe (Int -> Cow)

-- cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString n v g = (fmap Cow (noEmpty n))<*>(noNegative v)<*> (noNegative g)

-- test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

-- newtype Name = Name String deriving (Eq, Show)
-- newtype Address = Address String deriving (Eq, Show)

-- data Person = Person Name Address
--     deriving (Eq, Show)

-- validateLength :: Int -> String -> Maybe String
-- validateLength 0 _ = Nothing
-- validateLength n s 
--         | n> length s = Just s
--         |otherwise = Nothing

-- test31 = validateLength 5 "abc" == Just "abc"
-- mkName :: String -> Maybe Name
-- mkName s =  fmap Name (validateLength  25 s)

-- mkAddress :: String -> Maybe Address
-- mkAddress s = fmap Address (validateLength 100 s) 

-- test32 = mkName "Gigel" ==  Just (Name "Gigel")
-- test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

-- mkPerson :: String -> String -> Maybe Person
-- mkPerson s1 s2 = fmap Person (mkName s1) <*> (mkAddress s2)

-- test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))



{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

fct2 :: Maybe Int -> Maybe Bool
fct2 mx = do
    x<-mx
    if x<0 then return (False)
    else return ( True)


addM1 :: Maybe Int -> Maybe Int -> Maybe Int
addM1 mx my = mx >>= (\x -> my >>= (\y-> Just (x+y)))

addM2 :: Maybe Int -> Maybe Int -> Maybe Int
addM2 _ Nothing = Nothing
addM2 Nothing _ = Nothing
addM2 (Just x) (Just y) = Just (x+y)

addM3 :: Maybe Int -> Maybe Int -> Maybe Int
addM3 mx my = do
              x<- mx
              y<- my
              return (x+y)


cartesian_product xs ys = xs >>= ( \x -> ys >>= \y-> return (x,y))

cartesianProduct_do xs xy = do
                            x<-xs
                            y<- xy
                            return (x,y)
prod f xs ys = [f x y | x <- xs, y<-ys]

prod_do f xs ys = do
                x<-xs
                y<- ys
                return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine_do ::IO String
myGetLine_do = do
               x<- getChar
               if x== '\n' then return []
               else
                  do
                         xs<-myGetLine_do
                         return (x:xs)


prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

-- ioNumber2 = readLn >>= (\noin ->(putStrLn $ "Intrare\n" ++ (show noin))>>= (_\-> let noout = prelNo noin in(putStrLn $ "iesire" >>= (_\-> print noout))))


data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN = undefined
showPersonA :: Person -> String
showPersonA = undefined

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson = undefined

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap f ma = pure f <*> ma

mshowPersonN ::  Reader Person String
mshowPersonN = undefined
mshowPersonA ::  Reader Person String
mshowPersonA = undefined
mshowPerson ::  Reader Person String
mshowPerson = undefined
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}




data Point = Pt [Int]
    deriving Show

data Arb = Empty | Node Int Arb Arb
    deriving Show

class ToFromArb a where
 toArb :: a -> Arb
 fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt (h:t)) = ajutator h (toArb (Pt t))
    fromArb Empty = Pt []
    fromArb (Node x left right) = Pt (ajutator2 (fromArb left)++ [x] ++ ajutator2 (fromArb right) )

ajutator2 ::Point -> [Int]
ajutator2 (Pt [])= []
ajutator2 (Pt list) = list

ajutator :: Int ->Arb  -> Arb
ajutator x Empty = Node x Empty Empty
ajutator x (Node node left right )
        |x< node = Node node  (ajutator x left ) right
        |otherwise = Node node left (ajutator x right)

getFromInterval1:: Int -> Int ->[Int] -> [Int]
getFromInterval1 x y [] =[]
getFromInterval1 x y (h:t)
    |x<=h && y>=h = h:(getFromInterval1 x y t)
    |otherwise = getFromInterval1 x y t

getFromInterval2 ::Int -> Int ->[Int] -> [Int]
getFromInterval2 x y xs = do
                            h <- xs
                            if x<=h && y>=h then return h
                            else []

newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}

instance Monad (ReaderWriter env) where
  return x = RW (\_ -> (x,""))
  ma >>= k = RW f
    where f env = let (a,s1) = getRW ma env
                      (b,s2) = getRW (k a) env
                  in  getRW (RW(\_ ->(b,s1++s2))) env


instance Applicative (ReaderWriter env) where
  pure :: a -> ReaderWriter env a
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (ReaderWriter env) where              
  fmap f ma = pure f <*> ma    


newtype WriterM a = MW {getMW :: (Either String a,String)}


instance  Monad WriterM where
  return va = MW (Right va, "")
  ma >>= k = let (va, log1) = getMW ma
         in
         case va of
            Left err -> MW(Left err, log1)
            Right val -> let (vb, log2) = getMW (k val)
                    in MW (vb, log1 ++ log2)

instance Applicative (WriterM ) where
  pure :: a -> WriterM a
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (WriterM ) where              
  fmap f ma = pure f <*> ma    


testWriterM :: WriterM Int 
testWriterM = ma >>= k 
 where 
 ma = MW (Left "mama", "ana are mere ")
 k x = MW (if x `mod` 2==0 then Right x else Left "nu", "si pere!" )
