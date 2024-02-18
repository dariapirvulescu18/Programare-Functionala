data Tree = Empty  -- arbore vid
   | Node Int Tree Tree Tree -- arbore cu valoare de tip Int in radacina si 3 fii
      
extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

class ArbInfo t where
  level :: t -> Int -- intoarce inaltimea arborelui; pt un arbore vid
                      -- se considera ca are inaltimea 0
  sumval :: t -> Int -- intoarce suma valorilor din arbore
  nrFrunze :: t -> Int -- intoarce nr de frunze al arborelui
-- level extree
-- 3
-- sumval extree
-- 13
-- nrFrunze extree
-- 2
instance ArbInfo Tree where
    level Empty = 0
    level (Node _ t1 t2 t3) = 1 + max (level t1) (max (level t2) (level t3)) 
    sumval Empty =0
    sumval (Node x t1 t2 t3)= x+ sumval t1 + sumval t2 + sumval t3
    nrFrunze Empty =0
    nrFrunze (Node _ Empty Empty Empty) = 1
    nrFrunze (Node _ son1 son2 son3) = nrFrunze son1 + nrFrunze son2 + nrFrunze son3

class Scalar a where
  zero :: a
  one :: a
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a -- adunare vector
  smult :: a -> v a -> v a  -- inmultire cu scalare
  negatev :: v a -> v a -- negare vector

instance Scalar Double where
  zero = 0
  one = 1
  adds x y = x + y
  mult x y = x * y
  negates x = (-x)
  recips x = 1 / x

data Vector2D a = V2D a a

instance (Scalar a) => Vector Vector2D a where
  zerov = V2D zero zero
  onev = V2D one zero
  addv (V2D a b) (V2D c d) = V2D (adds a c) (adds b d)
  smult a (V2D b c) = V2D (mult a b) (mult a c)
  negatev (V2D a b) = V2D (negates a) (negates b)

data Vector3D a = V3D a a a

instance (Scalar a) => Vector Vector3D a where
  zerov = V3D zero zero zero
  onev = V3D one zero zero
  addv (V3D a b c) (V3D d e f) = V3D (adds a d) (adds b e) (adds c f)
  smult a (V3D b c d) = V3D (mult a b) (mult a c) (mult a d)
  negatev (V3D a b c) = V3D (negates a) (negates b) (negates c)
  