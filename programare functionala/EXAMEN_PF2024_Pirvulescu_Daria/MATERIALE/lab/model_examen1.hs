import Data.Char
import System.Win32 (COORD(xPos))
data Expr = Var String | Val Int | Plus Expr Expr | Mult Expr Expr
    deriving (Show, Eq)

class Operations exp where
 simplify :: exp -> exp

instance Operations Expr where
    simplify (Var x) = Var x
    simplify (Val x) = Val x
    simplify (Plus x (Val 0)) = simplify x
    simplify (Plus (Val 0) x) = simplify x
    simplify (Mult x (Val 0)) = Val 0
    simplify (Mult (Val 0) x) = Val 0
    simplify (Mult (Val 1) x) = simplify x
    simplify (Mult x (Val 1)) = simplify x
    simplify (Plus x y) = let xsimpli = simplify x
                              ysimpli = simplify y
                           in if xsimpli /= x || ysimpli /= y then simplify (Plus xsimpli ysimpli) else Plus xsimpli ysimpli
    simplify (Mult x y) = let xsimpli = simplify x
                              ysimpli = simplify y
                        in if xsimpli /= x || ysimpli /= y then simplify (Mult xsimpli ysimpli) else Mult xsimpli ysimpli
ex1 = Mult (Plus (Val 1) (Var "x")) (Val 1)
ex2 = Plus ex1 (Val 3)
ex3 = Plus (Mult (Val 0) (Val 2)) (Val 3)
ex4 = Mult ex3 (Val 5)

pasareasca::String -> String
pasareasca []= []
pasareasca (h:t)
   |elem h "aeiouAEIOU"= h:(pasareasca t)
   |otherwise = if isAlpha (h) then h:'P':h:(pasareasca t) else h:(pasareasca t)

--solutie cu monada
pasareascam::String -> String
pasareascam xs = do
       x <- xs
       if elem x "aeiouAEIOU" then return x
       else if isAlpha x then x:"P"++[x]
            else return x



newtype ReaderM env a = ReaderM { runReaderM :: env -> Either String a }

instance Functor (ReaderM env) where
    fmap f ma = pure f <*> ma

instance Applicative (ReaderM env) where
    pure x = ReaderM $ \env -> Right x
    mf <*> ma = do
                    f <- mf
                    a <- ma
                    return (f a)

instance Monad (ReaderM env) where
    return x = ReaderM $ \_ -> Right x
    ma >>= k = ReaderM $ \env ->
        case runReaderM ma env of
            Left err -> Left err
            Right val -> runReaderM (k val) env

testReaderM :: ReaderM String String
testReaderM = ma >>= k where ma = ReaderM (\ str -> if length str > 10 then Right (length str) else Left "")
                             k val = ReaderM (\ str -> if even val then Right "par" else Left "")

main :: IO ()
main = do
    let result = runReaderM testReaderM "Exemplu de test pentru readerM"
    case result of
        Left err -> putStrLn $ "Eroare: " ++ err
        Right val -> putStrLn $ "Rezultat: " ++ val