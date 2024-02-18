import System.Win32 (xBUTTON1, aCCESS_SYSTEM_SECURITY, COORD (xPos))
data Point = Pt [Int]
         deriving Show

data Arb = Empty | Node Int Arb Arb
            deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt xs) = foldl insertArb Empty xs
      where
            insertArb::Arb ->Int ->Arb
            insertArb Empty x = Node x Empty Empty
            insert (Node val left right ) x 
                | x <= val = insertArb left x
                | otherwise = insertArb right x
    
    fromArb a = Pt $ inorder  a
        where 
            inorder ::Arb ->[Int]
            inorder Empty = []
            inorder (Node val left right ) = (inorder left) ++ [val] ++ (inorder right)


-- getFromInterval 5 7 [1..10] == [5,6,7]
--fara monada
getFromInterval :: Int -> Int ->[Int] ->[Int]
getFromInterval _ _ [] = []
getFromInterval x y (h:t)
    | y<h || x>h = (getFromInterval x y t)
    | otherwise = h:(getFromInterval x y t)

--cu monada
getFromIntervalm :: Int -> Int ->[Int] ->[Int]
getFromIntervalm x y xs = do
    v <- xs
    if v>=x && v <=y 
        then return v
        else  []

instance  Functor (ReaderWriter env ) where              
  fmap f ma = pure f <*> ma     

newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}
instance Applicative (ReaderWriter env) where
    pure x = RW $ \env -> (x,"")
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)      

instance  Monad (ReaderWriter env) where              
    return = pure
    ma >>= f = RW $ \env ->
        let (va,log1) = getRW ma env
            (vb, log2) = getRW (f va) env
        in (vb, log1 ++log2)

