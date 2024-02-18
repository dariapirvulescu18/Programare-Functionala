data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"

evalExp :: Expr -> Int
evalExp (Const x)= x
evalExp (e1:+:e2)=(evalExp e1) +(evalExp e2)
evalExp (e1:*:e2)=(evalExp e1) *(evalExp e2)

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16


evalArb :: Tree -> Int
evalArb (Lf x) =x
evalArb (Node Add x y)= evalArb x + evalArb y
evalArb (Node Mult x y)= evalArb x * evalArb y


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0) (Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3) (Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3) (Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3) (Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


expToArb :: Expr -> Tree
expToArb (Const x)= Lf x
expToArb (e1:+:e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1:*:e2) = Node Mult (expToArb e1) (expToArb e2)

expresie = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
test = expToArb expresie == Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0) (Lf 5))


data IntSearchTree value
  = Empty
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare
      deriving(Eq,Show)

tree = BNode (BNode Empty 1 (Just 'a') Empty) 2 ( Just 'b') (BNode Empty 4 (Just 'd') Empty)
tree2=Empty

lookup' :: Int -> IntSearchTree value -> Maybe value
lookup'  x  Empty = Nothing
lookup' x (BNode left k value right )
        | x==k = value
        | x<k = lookup' x left
        | otherwise = lookup' x right


keys ::  IntSearchTree value -> [Int]
keys Empty =[]
keys (BNode left k value right ) = (keys left) ++[k] ++(keys right)



values :: IntSearchTree value -> [value]
values Empty = []
values (BNode left k Nothing right )  = (values left) ++(values right)
values (BNode left k (Just v) right )  = (values left) ++ [v]++(values right)


-- sau pot sa fac cu functia fromMaybe


insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert cheie val Empty = BNode Empty cheie (Just val) Empty
insert cheie val (BNode left key value right)
    | cheie <= key = BNode (insert cheie val left) key value right
    | otherwise = BNode left key value (insert cheie val right)

delete :: Int -> IntSearchTree value -> IntSearchTree value
delete cheie (Empty) = Empty
delete cheie (BNode left key val right)
    | key == cheie = BNode left cheie Nothing right
    | cheie <= key = BNode (delete cheie left) key val right
    | otherwise = BNode left key val (delete cheie right)


-- 9. Scriet,
-- i o funct,
-- ie care întoarce lista elementelor dintr-un arbore de cautare. Hint: aten ˘ t,
-- ie la
-- Maybe!


toList :: IntSearchTree value -> [(Int, value)]
toList Empty =[]
toList (BNode left key Nothing right )=(toList left ) ++(toList right)
toList (BNode left key (Just val) right) =(toList left ) ++[(key, val)] ++(toList right)





fromList2 :: [(Int, value)] -> IntSearchTree value
fromList2 [] = Empty
fromList2 ((key,value):t) = insert key value .fromList2 $ t


fromList :: [(Int,value)] -> IntSearchTree value
fromList lista = foldr (\(x,y) acc ->insert x y acc) Empty lista

printTree :: Show value => IntSearchTree value -> String
printTree Empty = ""
printTree (BNode left key Nothing right ) ="(" ++(printTree left ) ++ ")"  ++ (show key) ++ "(" ++(printTree right ) ++ ")"
printTree (BNode left key (Just val) right ) ="(" ++ (printTree left ) ++ ")"  ++ (show key) ++ ":"  ++ show val ++ "(" ++(printTree right ) ++ ")"

-- balance :: IntSearchTree value -> IntSearchTree value
-- balance = undefined
