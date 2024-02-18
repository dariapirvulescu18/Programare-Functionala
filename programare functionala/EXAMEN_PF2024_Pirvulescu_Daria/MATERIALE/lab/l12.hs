import Data.Binary.Get (label)
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
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x lista) = Cons (f x) (fmap f lista)
instance Applicative List where
    pure f = Cons f Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f sec) lista = concatenare (fmap f lista) ( (<*>) sec lista)

concatenare::List a -> List a -> List a
concat Nil l = l
concatenare (Cons x l1) l2 = Cons x (concatenare l1 l2)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty orice = Just orice

noNegative :: Int -> Maybe Int
noNegative x = if x<0 then Nothing else Just x

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing
test23 = noNegative 5 == Just 5

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString nume varsta greutate
    | noEmpty nume == Nothing || noNegative varsta == Nothing || noNegative greutate == Nothing = Nothing
    | otherwise = Just Cow {name = nume, age = varsta, weight = greutate}
test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

cowFromString_c :: String ->Int ->Int ->Maybe Cow
cowFromString_c n v g = fmap Cow (noEmpty n) <*> (noNegative v) <*> (noNegative g)

test25 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})
newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x l
    | length l < x = Just l
    | otherwise = Nothing

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName nume = fmap Name (validateLength 25 nume)

mkAddress :: String -> Maybe Address
mkAddress adresa = fmap Address (validateLength 100 adresa)

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson nume adresa  = fmap Person (mkName nume) <*> mkAddress adresa
-- sau Person <*> (mkName nume) <*> (mkAddress adresa)  
test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))
