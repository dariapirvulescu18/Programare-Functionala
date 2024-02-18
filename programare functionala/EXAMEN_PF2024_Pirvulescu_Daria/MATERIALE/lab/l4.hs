{-
[x^2 |x <- [1..10], x `rem` 3 == 2]
[(x,y) | x <- [1..5], y <- [x..(x+2)]]
[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
[[x..y] | x <- [1..5], y <- [1..5], x < y]
-}
factori :: Int -> [Int]
factori  x =[y| y<- [1..x], x `rem` y ==0]

prim :: Int -> Bool
prim x
     | factori x ==[1,x] = True
     | otherwise = False

prim2 :: Int -> Bool
prim2 x = length (factori x) == 2

numerePrime :: Int -> [Int]
numerePrime x = [y| y <- [2..x], prim2 y==True ]

flaten:: [((a,b),c)] -> [(a,b,c)]
flaten lista = [(a,b,c)| ((a,b),c) <-lista]

myzip3 ::[a] ->[b] ->[c]->[(a,b,c)]
myzip3 l1 l2 l3= flaten ( zip (zip l1 l2) l3 )


firstEl::[(a,b)] ->[a]
firstEl = map fst

sumList::[[Int]]->[Int]
sumList = map sum

prel2:: [Int] ->[Int]
prel2 = map (\x -> if mod x 2==0 then x `div` 2 else x*2)

caracter:: Char ->[[Char]] ->[[Char]]
caracter a lista = filter ( elem a) lista

patrate:: [Int] ->[Int]
patrate l= map (^2) $ filter (\x ->mod x 2 ==1)  l

pozImp :: [Int] ->[Int]
pozImp lista = map (\(x,i) -> x^2 ) (filter (\(x,i)-> even i) ( zip [1..] lista))

eliminareC:: [Char] ->[Char]
eliminareC lista= [a| a<-lista, elem a "aeiouAEIOU"]

numaiVocale::[[Char]]->[[Char]]
numaiVocale =map eliminareC

mymap::(a->b)->[a]->[b]
mymap f [] =[]
mymap f (h:t)= (f h): (mymap f t)

myfilter ::(a->Bool) -> [a] ->[a]
myfilter f [] =[]
myfilter f (h:t)
      | f h = h:myfilter f t
      |otherwise = myfilter f t
