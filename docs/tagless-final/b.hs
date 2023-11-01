{-# LANGUAGE GADTs #-}

module B where

data TIExp t where
    ConstInt :: Int -> TIExp Int
    ConstBool :: Bool -> TIExp Bool
    Add :: TIExp Int -> TIExp Int -> TIExp Int
    Neg :: TIExp Int -> TIExp Int
    If :: TIExp Bool -> TIExp a -> TIExp a -> TIExp a

eval :: TIExp a -> a
eval (ConstInt i) = i
eval (ConstBool b) = b
eval (Neg e) = -(eval e)
eval (Add e0 e1) = eval e0 + eval e1
eval (If e0 e1 e2) = if eval e0 then eval e1 else eval e2

main = print $ eval $ Neg $ ConstInt 290
