{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Control.Monad.State
import Data.Functor.Constant (Constant)
import Data.Map.Strict qualified as Map

class Exp repr where
  int :: Int -> repr Int
  bool :: Bool -> repr Bool
  add :: repr Int -> repr Int -> repr Int
  neg :: repr Int -> repr Int
  if_ :: repr Bool -> repr a -> repr a -> repr a
  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b

newtype Wrap a = Wrap {unWrap :: a}

instance Exp Wrap where
  int = Wrap
  bool = Wrap
  add (Wrap i0) (Wrap i1) = Wrap $ i0 + i1
  neg (Wrap i) = Wrap $ -i
  if_ (Wrap b) e0 e1 = if b then e0 else e1
  lam f = Wrap (unWrap . f . Wrap)
  app (Wrap f) (Wrap e) = Wrap $ f e

{-
newtype View a = View String

instance Exp View where
    int = View . show
    bool = View . show
    add (View i0) (View i1) = View $ i0 ++ " + " ++ i1
    neg (View i) = View $ "-" ++ i
    if_ (View b) (View e0) (View e1) = View $ "if " ++ b ++ " then " ++ e0 ++ " else " ++ e1
    lam f =
        let (View fs) = f (View "x") in
        View $ "\\x -> " ++ fs
    app (View f) (View e) = View $ f ++ " " ++ e
-}

newtype View a = View (State Int String)

instance Exp View where
  int i = View $ return $ show i
  bool b = View $ return $ show b
  add (View e0) (View e1) =
    View $ do
      i0 <- e0
      i1 <- e1
      return (i0 ++ " + " ++ i1)

  neg (View e) =
    View $ do
      i <- e
      return ("-" ++ i)

  if_ (View e0) (View e1) (View e2) =
    View $ do
      b <- e0
      v0 <- e1
      v1 <- e2
      return ("if " ++ b ++ " then " ++ v0 ++ " else " ++ v1)

  lam f =
    View $ do
      c <- get
      let var = "x" ++ show c
      let (View fb) = f (View (return var))
      put (c + 1)
      b <- fb
      return ("(\\" ++ var ++ " -> " ++ b ++ ")")

  app (View f) (View e) =
    View $ do
      fn <- f
      exp <- e
      return (fn ++ " " ++ exp)

view :: View a -> String
view (View exp) = evalState exp 0

data ConstantFold repr a where
  CInt :: Int -> ConstantFold repr Int
  CBool :: Bool -> ConstantFold repr Bool
  CRepr :: repr a -> ConstantFold repr a

prjCF :: (Exp repr) => ConstantFold repr a -> repr a
prjCF (CInt i) = int i
prjCF (CBool b) = bool b
prjCF (CRepr r) = r

instance (Exp repr) => Exp (ConstantFold repr) where
  int = CInt
  bool = CBool
  add (CInt i0) (CInt i1) = CInt $ i0 + i1
  add i0 i1 = CRepr $ add (prjCF i0) (prjCF i1)
  neg (CInt i) = CInt (-i)
  neg i = CRepr $ neg $ prjCF i
  if_ (CBool True) e0 e1 = e0
  if_ (CBool False) e0 e1 = e1
  if_ b e0 e1 = CRepr $ if_ (prjCF b) (prjCF e0) (prjCF e1)
  lam f = CRepr $ lam (prjCF . f . CRepr)
  app f e = CRepr $ app (prjCF f) (prjCF e)

eval :: ConstantFold View a -> String
eval = view . prjCF

expr0 :: ConstantFold View Int
expr0 = add (int 2) (int 3)

expr1 :: ConstantFold View (Int -> Int)
expr1 = lam (\x -> add x (add (int 2) (int 3)))

expr2 :: View (Int -> Int)
expr2 = lam (\y -> add y (add (app (lam (\x -> add x (int 3))) (add (int 5) (int 2))) (app (lam (\x -> add x (int 2))) (int 1))))

data BetaNormalize repr a where
  BNLam :: (repr a -> repr b) -> BetaNormalize repr (a -> b)
  BNRepr :: repr a -> BetaNormalize repr a

prjBN :: (Exp repr) => BetaNormalize repr a -> repr a
prjBN (BNLam f) = lam f
prjBN (BNRepr r) = r

instance (Exp repr) => Exp (BetaNormalize repr) where
  int i = BNRepr $ int i
  bool b = BNRepr $ bool b
  add i0 i1 = BNRepr $ add (prjBN i0) (prjBN i1)
  neg i = BNRepr $ neg $ prjBN i
  if_ b e0 e1 = BNRepr $ if_ (prjBN b) (prjBN e0) (prjBN e1)
  lam f = BNLam $ prjBN . f . BNRepr
  app (BNLam f) e = BNRepr $ f (prjBN e)
  app (BNRepr f) e = BNRepr $ app f (prjBN e)

eval' :: ConstantFold (BetaNormalize View) a -> String
eval' = view . prjBN . prjCF

expr3 :: ConstantFold (BetaNormalize View) (Int -> Int)
expr3 = lam (\y -> add y (add (app (lam (\x -> add x (int 3))) (add (int 5) (int 2))) (app (lam (\x -> add x (int 2))) (int 1))))

eval'' :: BetaNormalize (ConstantFold View) a -> String
eval'' = view . prjCF . prjBN

expr4 = lam (\y -> add y (add (app (lam (\x -> add x (int 3))) (add (int 5) (int 2))) (app (lam (\x -> add x (int 2))) (int 1))))

main = do
  print $ eval expr0
  print $ eval expr1
  print $ view expr2
  print $ eval' expr3
  print $ eval'' expr4
