newtype AE a = AE {getAE::(Either String a, String )} deriving Show

instance  Monad AE where
  return va = AE (Right va, "")
  ma >>= k = let (va, log1) = getAE ma
         in
         case va of
            Left err -> AE(Left err, log1)
            Right val -> let (vb, log2) = getAE (k val)
                    in AE(vb, log1 ++ log2)


instance Functor AE where
    fmap f ma = f <$> ma
instance Applicative AE where
    pure = return
    mf <*> ma = do
        f <- mf
        f <$> ma

testAE::AE Int
testAE = ma>>=f
    where 
        ma= AE (Right 7, "ana are mere")
        f x = AE (if x `mod` 2==0 then Right x else Left "error", "si pere!")

