import B hiding (main)
import C hiding (main)


i2f (ConstInt i) = constint i
i2f (ConstBool b) = constbool b
i2f (Add i0 i1) = add i0 i1
i2f (Neg i) = neg i
i2f (If e0 e1 e2) = if_ e0 e1 e2

instance TFExp TIExp where
    constint = ConstInt
    constbool = ConstBool
    add = Add
    neg = Neg
    if_ = If

f2i :: TIExp t -> TIExp t
f2i = id

check = f2i . i2f


main = do
    print $ B.eval $ check $ Neg $ ConstInt 10
    -- print $ C.eval $ check1 $ neg $ constint 10
