# Tagless-final は難しい

## Tagless-final とは
DSL を実装するとき, 一般には AST を書くと思いますが,
特殊な実装手法として Tagless-final がありまして,
最近触っていたので紹介します.

### (Tagged-)Initial
まずは一般的な AST による実装方法を考えてみます.
例として Int と Bool があって, 加算と符号反転とif分岐がある DSL を考えます.
以下は Haskell による実装例です.

```haskell
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
```
複数の種類の値を含む言語では,
加算や符号反転は Int 同士でないといけない反面条件分岐では Bool が来ないといけない等,
式に制約があります.
こういういわゆる型を表現するために評価した値にタグ付けが必要になってくると思います.
例えば加算の場合は両方の引数を評価した値が共に Int であるかを確認し,
そうでなければエラーを送出するみたいなことをするわけですね.
上の例ではエラーを Maybe 型で表現し, エラーの場合は Nothing が返るようになっています.

### Tagless-initial
GADT を使うと, このタグ付けが必要なくなります. すごい!

```haskell
{-# LANGUAGE GADTs #-}

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
```

最初の例だと型制約に違反するような式を書くことができ,
評価時に型検査をしていましたが,
この例では DSL の型検査をホスト言語 haskell にやってもらっているので,
そもそも型制約に違反する式は書けなくなります.

この表現方法では値にタグが必要なくなることから Tagless,
また圏論の用語の initial[^tagless-initial]から Tagless-initial と呼ばれています.

## Tagless-final
initial スタイルではコンストラクタとして表現していた式を,
Tagless-final スタイルでは関数として表します.
分かる人には式を denotational に書くと言うと分かりやすいかもしれません.

```haskell
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
```

`newtype` は型シノニムで, haskell のコンパイル時には除去されます.
今回は Int と Bool の両方の値を表現する型として用意しています.

この場合もホスト言語が型検査をしてくれます.
人によっては Wrap が健全性の構築的証明に見えるかもしれません[^typed-tagless-final].

tagless はともかく, final は initial と対称的に見えることから名付けられたらしいです(えぇ...).
圏論的に終対象では無くむしろ tagless-initial と tagless-final は全単射ぽいです.
相互の変換が容易に書けそう, ですよね？

```haskell
initial_to_final (ConstInt i) = constint i
:

instance TFExp TIExp where
    constint = ConstInt
    :

final_to_initial :: TIExp t -> TIExp t
final_to_initial = id
```

## Tagless-final の活用
単なる評価や式の文字列への変換はもちろん, 最適化を始めとするいろんな変換が書けます.
Oleg 先生の cookbook[^cookbook] を見ると高度な例がたくさん並んでいて, 例えば

- CPS 変換
- 評価戦略 (call-by-name, call-by-value, call-by-need) の実装
- de Bruijn index の lambda 計算が(引数に関して)閉じているか判定
- CSE 除去

はできます.

### Tagless-final 活用例
活用例を出しておきます. 上の例を少し拡張して関数抽象 lam と 関数適用 app を加えます.

```haskell
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
```

#### 文字列
今度は式を評価する前の文字列を得ることを考えます.
Exp の各関数に対して文字列への変換を定義してあげればいいんですが,
lam を導入したことで fresh な変数をどうやって得るかという問題が出てきます.

今回は State モナドを噛ませて実装してみました.
lam の引数の変数が x0, x1 というように順に生成されて出力されるようになっています.

```haskell
import Control.Monad.State
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
```

括弧の有無が適当ですがまぁ置いておきます.

#### 定数畳み込み
次にこの式の定数畳み込みによる最適化を考えてみます.
上の例ではプリミティブな値へ変換すれば良かったのですが,
今度はあくまで最適化なので得たいものは式です.

出来上がったのがこちらになります.

```haskell
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
```

ややこしいですが, やってることとしては
- 定数畳み込みをするのに必要な情報を持つ中間状態 `ConstantFold` を定義する
  - ここでは部分式に関数抽象や適用を含まない場合だけ評価する
    - CInt が部分式を評価すると Int 定数になることを表現している
    - CRepr は畳み込みできず, そのまま式を保持する
- DSL から `ConstantFold` へ変換するインスタンスを書く
  - 例えば If の条件が Bool 定数 True の場合は then 節の式だけを持ち, 他は捨てる
- `ConstantFold` から `repr` への変換関数 `prjCF` を実装する

```haskell
eval :: ConstantFold View a -> String
eval = view . prjCF

expr :: View (Int -> Int)
expr = lam (\y -> add y (add (add (int 5) (int 2)) (int 2)))

str = eval expr
-- "(\x0 -> x0 + 9)"
-- 部分式 add (add (int 5) (int 2)) (int 2) が畳み込まれて 9 に評価されている
```

#### ベータ簡約
次の例はベータ簡約です.
`app` の第一引数が `lam` だけ簡約できるので,
中間状態として `lam` に相当するものを持てば良いんですね.

```haskell
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
```

#### 合成
先に定義した定数畳み込みとベータ簡約は,
型クラス Exp に属していれば定義されます.
つまり順不同で自由に組み替えられます.

```haskell
eval' :: ConstantFold (BetaNormalize View) a -> String
eval' = view . prjBN . prjCF

expr' :: ConstantFold (BetaNormalize View) (Int -> Int)
expr' = lam (\y -> add y (add (app (lam (\x -> add x (int 3))) (add (int 5) (int 2))) (app (lam (\x -> add x (int 2))) (int 1))))

str' = eval' expr'
-- "(\x0 -> x0 + 7 + 3 + 1 + 2)"

eval'' :: BetaNormalize (ConstantFold View) a -> String
eval'' = view . prjCF . prjBN

expr'' = lam (\y -> add y (add (app (lam (\x -> add x (int 3))) (add (int 5) (int 2))) (app (lam (\x -> add x (int 2))) (int 1))))

str'' = eval'' expr''
-- "(\x0 -> x0 + 13)"
```

この場合は, 定数畳み込みを先にやってしまうとベータ簡約した後に更に畳み込める部分式が現れる可能性があるので,
ベータ簡約を先にやった方が良いですね.

こういうふうに変換をガチャガチャ組み替えられるのは便利で綺麗ですね (別に Tagless-initial でもできますが).

## 所感
活用例を見て便利さや可能性は感じてもらえたかもしれません.
しかし暫く格闘してみて感じた厳しさを挙げます.

- パッと見何やってるのか分からない

  可読性が低いわけではないですが, 追うのは比較的難しいです.
  原因としては
  - Exp のインスタンスがそれ単体では中間表現にしか変換せず, 中間表現から別のインスタンスへの変換 prj も含めて追う必要がある
  - Exp のインスタンスが別の Exp インスタンスの関数を含みがち
    - add 中に別の add が出てくる例 `add i0 i1 = BNRepr $ add (prjBN i0) (prjBN i1)`

  とかですかねぇ

- 中間表現って何？

  ある変換をするのに必要な中間表現というのが曲者で, これを思いつくのが難しいんですね[^denotational].
  なにかうまい変換を思いつけば軽い論文になる気がします.

- 型検査をホスト言語に任せているせいで型情報が取れない

  型検査をしなくて良いというのはメリットなんですが, 変数が絡む場合, DSL の部分式が特定の型の場合だけ変換したい
  みたいな状況で, 型情報のが無くて困ったりします.
  そういう場合, 中間表現として AST を考えて, AST から repr へ変換する際に型検査するとまぁ出来はするんですが,
  最初から Tagless-Initial で書けば良いじゃんみたいな感じになります.

逆に良いところとしては, haskell の場合は型クラスで DSL の項が定義されるので,
別の型クラスを定義して項を拡張できて, 拡張性に優れるとかですかね.
ocaml だと module functor を使いますが, 同様に AST を書くよりは拡張しやすいと思います.

# Footnote
[^tagless-initial]: <https://serokell.io/blog/introduction-tagless-final> DSLで表現したいことは全てASTで表現できる(AST から変換できる, AST が始対象)ってことか？
[^typed-tagless-final]: <https://okmij.org/ftp/tagless-final/course/lecture.pdf>
[^cookbook]: <https://okmij.org/ftp/tagless-final/cookbook.html>
[^denotational]: 表示的意味論が使われなくなった(要出典)のに通づる気がする. 人類には早すぎたかもしれない
