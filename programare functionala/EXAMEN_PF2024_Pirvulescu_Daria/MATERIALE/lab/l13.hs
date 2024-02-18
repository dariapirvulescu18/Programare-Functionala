{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
import System.Win32 (COORD(xPos))
{- Monada Maybe este definita in GHC.Base 

( > >=) : : m a −> ( a −> m b ) −> m b

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

fctdo::Maybe Int ->Maybe Bool
fctdo mx = do
            x <- mx
            return (pos x)

            
addMa :: Maybe Int -> Maybe Int -> Maybe Int
addMa mx my = mx >>= (\x -> my >>= (\ y -> Just(x+y)))

addMa2 :: Maybe Int -> Maybe Int -> Maybe Int   
addMa2 Nothing _ = Nothing
addMa2 _ Nothing = Nothing
addMa2 (Just x) (Just y) = Just(x+y)

addMb :: Maybe Int -> Maybe Int -> Maybe Int
addMb mx my =  do
                x <- mx
                y <- my
                return(x+y)
cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product_do xs xy = do
                            x <- xs
                            y <- xy
                            return (x,y)
prod f xs ys = [f x y | x <- xs, y<-ys]
prod_do f xs ys = do
                x <- xs
                y <- ys
                return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine_do :: IO String
myGetLine_do = do
              x <- getChar
              if x == '\n' then
                return []
              else 
                do
                   xs <-myGetLine_do
                   return (x:xs)
prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

ioNumber2 = readLn >>= (\noin -> (putStrLn ("Intrare\n" ++ (show noin)) >>= (\_ -> let noout = prelNo noin in (putStrLn "Iesire" >>= (\_ ->print noout)))))

--- Monada Writer

-- newtype WriterS a = Writer { runWriter :: (a, String) } 
--                     deriving Show


-- instance  Monad WriterS where
--   return va = Writer (va, "")
--   ma >>= k = let (va, log1) = runWriter ma
--                  (vb, log2) = runWriter (k va)
--              in  Writer (vb, log1 ++ log2)


-- instance  Applicative WriterS where
--   pure = return
--   mf <*> ma = do
--     f <- mf
--     a <- ma
--     return (f a)       

-- instance  Functor WriterS where              
--   fmap f ma = pure f <*> ma     

-- tell :: String -> WriterS () 
-- tell log = Writer ((), log)
  
-- logIncrementN :: Int -> Int -> WriterS Int
-- logIncrementN x n = do
--                 tell("increment: " ++ show x ++ "\n") 
--                 if n==1 then
--                     return (x+1)
--                 else
--                     logIncrementN (x+1)(n-1)

-- logIncrementx :: Int -> WriterS Int
-- logIncrementx x = do
--                    tell("increment: " ++ show x ++ "\n" )
--                    return (x+1)
   
                  

newtype WriterLS a = Writer {runWriter :: (a, [String])}
                    deriving Show
instance  Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterLS where
  pure = return
  (<*>) :: WriterLS (a -> b) -> WriterLS a -> WriterLS b
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterLS () 
tell log = Writer ((), [log])
  
logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x n = do
                tell("increment: " ++ show x) 
                if n==1 then
                    return (x+1)
                else
                    logIncrementN (x+1)(n-1)

data Person = Person { name :: String, age :: Int }
                deriving Show

showPersonN :: Person -> String
showPersonN x = "NAME: " ++ (name x) 
showPersonA :: Person -> String
showPersonA x = "AGE: " ++ (show(age x)) 

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson  x = "(" ++ showPersonN x ++ "," ++ showPersonA x ++ ")"  

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
mshowPersonN = Reader showPersonN
mshowPersonA ::  Reader Person String
mshowPersonA = Reader showPersonA 
mshowPerson ::  Reader Person String
mshowPerson = do
                nume <- mshowPersonN
                varsta <-mshowPersonA
                return ("("++ nume ++ "," ++ varsta ++ ")")
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}

para :: IO (Maybe String)
para = do
       x <- getLine 
       if even (length x) then return Nothing else return (Just "Par") 