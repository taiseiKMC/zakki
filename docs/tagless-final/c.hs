module C where

class TFExp repr where
    constint :: Int -> repr Int
    constbool :: Bool -> repr Bool
    add :: repr Int -> repr Int -> repr Int
    neg :: repr Int -> repr Int
    if_ :: repr Bool -> repr a -> repr a -> repr a

newtype Wrap a = Wrap {unWrap :: a}

instance TFExp Wrap where
    constint = Wrap
    constbool = Wrap
    add i0 i1 = Wrap $ unWrap i0 + unWrap i1
    neg i = Wrap $ -(unWrap i)
    if_ b e0 e1 = if unWrap b then e0 else e1

eval = unWrap

expr0 :: Wrap Bool
expr0 = if_ (constbool False) (if_ (constbool True) (constbool False) (constbool True)) (constbool True)

expr1 :: Wrap Int
expr1 = if_ (constbool True) (constint 1) (constint 2)
main = do
    print $ eval expr0
    print $ eval expr1
