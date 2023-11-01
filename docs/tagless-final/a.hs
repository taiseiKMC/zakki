data IExp =
    ConstInt Int
  | ConstBool Bool
  | Add IExp IExp
  | Neg IExp
  | If IExp IExp IExp

data IValue = VI Int | VB Bool deriving Show

eval :: IExp -> Maybe IValue
eval (ConstInt i) = return (VI i)
eval (ConstBool b) = return (VB b)
eval (Neg e) = do
  VI v <- eval e
  return $ VI $ -v
  
eval (Add e0 e1) = do
  VI i0 <- eval e0
  VI i1 <- eval e1
  return $ VI $ i0 + i1

eval (If e0 e1 e2) = do
  VB b <- eval e0
  v0 <- eval e1
  v1 <- eval e2
  return $ if b then v0 else v1
main = print $ eval $ Neg $ ConstBool True
