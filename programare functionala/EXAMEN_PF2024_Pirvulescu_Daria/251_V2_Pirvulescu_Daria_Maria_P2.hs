data Pereche a b = MyP a b deriving Show
data Lista a = MyL [a] deriving Show

class MyOp m where
    myFilter :: (a -> Bool)-> (b -> Bool) -> m (Pereche a b) -> m (Pereche a b)

instance MyOp Lista where
    myFilter _ _ (MyL []) = MyL []
    myFilter g f (MyL xs) = MyL (filter (\(MyP x y) -> g x && f y) xs)

lp :: Lista (Pereche Int Char)
lp = MyL [MyP 97 'a', MyP 3 'b', MyP 100 'd']
-- myFilter (<10) && (<'e') lp