# OCaml のあまり知られてなさそうな言語機能紹介
ここ数年 OCaml を書いていて,
こんなことできるんだ〜って思った言語機能についていくつか紹介します(というか以前紹介したのを改めてまとめます).
ここで紹介している機能もしていない機能も公式マニュアル <https://ocaml.org/manual/index.html> にあります.

(原題: [OCaml とかいう誰も知らん言語](https://github.com/taiseiKMC/ocaml-nobody-knows-slide/blob/master/ocaml-nobody-knows.pdf))

- Extensible Variant Type
- Polymorphic Variant
- Monadic bind
- PPX
- Substituting inside a Signature
- GADT

## Extensible Variant Type
通常ヴァリアント型は定義するときにそのコンストラクタを全て列挙しますが,
`type t = ..` という風にヴァリアント型を `..` (省略ではなくこう書きます)で定義すると,
ヴァリアント型のコンストラクタを後で追加することができます(!?).
追加する際は以下のように `+=` と書きます.

```ocaml
type t = ..
type t += Str of string
type t +=
    | Int of int
    | Float of float
```

mli にも書けますし module を跨ぐこともできますが,
他所でコンストラクタが追加される可能性が出てくる以上全列挙することはできなくなるので,
match 文にはワイルドパターンを必然的に書くことになります.

よく Result モナドの Error.t 型の方で使われているのを見ます.

```ocaml
module Error = struct
    type t = ..
    type t += Fatal of string
end
type 'a result = Ok of 'a | Error of Error.t

module Arith = struct
    type Error.t += Overflow | ZeroDivision
    let div a b = if b = 0 then (Error ZeroDivision) else Ok (a/b)
end

let from_ok = function
  | Ok x -> x
  | Error (Error.Fatal str) -> failwith str
  | Error Arith.Overflow -> failwith "Overflow"
  | Error Arith.ZeroDivision -> failwith "Zerodivision"
  | Error _ -> failwith "Unexpected"
```

## Polymorphic Variant
`` `T`` というように, 先頭にバッククォートを置いたコンストラクタを書くことができます.
このようなコンストラクタは通称タグと呼ばれていて, 特に宣言しなくても書けます
(`` `T 0`` と `` `T false`` のような同じ名前で違う型を持つタグが同時に出現すると怒られますが).
タグはどの型にも属していません.
タグに対する型としては出現しうるタグが列挙され, `[]` で囲まれて表記されます.
また ``type t = [...]`` のようにしてタグを列挙したものに名前を付けることができ,
型推論の助けにしたり pattern-match で便利に使うことができます.

```ocaml
type myvariant = [`Tag0 of int | `Tag1 of bool]
let f = function
    | #myvariant -> "myvariant" (* myvariant 型に属する tag 全てにマッチ *)
    | `Tag2 -> "Tag2"
======
val f : [< `Tag0 of int | `Tag1 of bool | `Tag2 ] -> string
```

`[]` 中の `<` はサブセットを表しています.
つまり, ``[< `Tag0 of int | `Tag1 of bool | `Tag2 ]`` は,
集合 ``{ `Tag0, `Tag1, `Tag2 }`` の部分集合にあたるタグの集合なら型推論が通るということですね.
逆に `>` はスーパーセットを表しています.

型が集合を扱う場合の当然の帰結として,
サブタイピングの判定を行う演算子 `:>` が用意されています.

```ocaml
type t = [`A | `B | `C]
let f = function
    | #t as t -> t
    | `D -> `E
let g = function
    | #t as t -> t
    | `D -> (`E :> [t | `D | `E])
let h = function
    | #t as t -> t
    | x -> x
======
val f : [< `A | `B | `C | `D ] -> [> `A | `B | `C | `E ] = <fun>
val g : [< `A | `B | `C | `D ] -> [> `A | `B | `C | `D | `E ] = <fun>
val h : ([> t ] as 'a) -> 'a = <fun>
```

一般にこのようなタグを列挙した型の推論は難しいので,
サブタイピングを含め型注釈を入れないと型検査がうまく行かないことがちょくちょくある印象です.

### Variance Annotations
さらに込み入った話として,
module abstract type に polymorphic variant を含む型が与えられたときにどう解釈するかという問題があります[^varianceAnnot].

例えば `module M : sig type 'a t end` とあったときに,
``[ `A ] M.t :> [ `A | `B ] M.t`` が成り立つかどうかを判定することはできるでしょうか？

`type 'a t = 'a` だった場合は成り立ちますが,
`type 'a t = 'a -> unit` だった場合は成り立ちません.
更に言えば, 前者は `'a :> 'b` ならば `'a t :> 'b t` で,
後者は `'a :> 'b` ならば `'b t :> 'a t` という関係が成り立ちます
(こういうのを "covariant" とか "contravariant" とかいうらしいですね).

そしてこの関係を `+` や `-` を型引数につけることで注釈することができます.
```ocaml
module M : sig
  type (-'a, +'b) t
end = struct
  type ('a, 'b) t = 'a -> 'b
end
```

## Monadic Bind
これまで OCaml で monad を書くと, よく bind 演算子祭りになってました.

```ocaml
let bind x f = match x with Some x -> f x | None -> None
let both x y = match (x, y) with Some x, Some y -> Some (x, y) | _ -> None
let return x = Some x
let (>>=) = bind

both (return 1) (return 2) >>= (fun (x, y) ->
return 3 >>= (fun z ->
return (x + y + z)))
```

上は option モナドによる例です.
複数行に渡って `>>=` が並ぶことになりますが,
慣れないと(慣れても)結合関係が分かりづらくて見辛いと思います.
もっとクリティカルな問題として,
型が合わない場合に全体(上の例の場合, 下三行)をエラー箇所として報告してきたりして非常に原因の特定に苦労しました.

monadic bind とは, let(symbol+) と and(symbol+) を演算子として定義できる機能です.
定義すると, let文っぽく書いて特殊な感じで展開してくれます.

例えば
```ocaml
let (let*) = bind
let (and*) = both

let* x = return 1 and* y = return 2 in
let* z = return 3 in
return (x + y + z)
```
と書くと,
```ocaml
(let*) ((and*) (return 1) (return 2)) (fun (x, y) ->
(let*) (return 3) (fun z ->
return (x + y + z)))
```
のように展開されます.
普通のlet文のように書いて読めるので,
構文糖衣のような機能の割に非常に可読性保守性が上がっている感じがして個人的に好きです.

## PPX[^ppx]
OCaml には Attribute, Extension Point などを埋め込むことができます.
ここでは深くは踏み込みませんので, 紹介以上に気になったら他の文献を見てください.

### Attribute
Attribute というのは `[@...]` のように,大括弧の中に 1-3個の `@` と payload[^payload] が入った形をしています.
`@` の個数によって修飾する対象が変わります.
`[@...]` は式に対する修飾, `[@@...]` は toplevel, つまり大体 let や module に対する修飾,
`[@@@...]` はファイル全体に対する修飾を表します.
他の言語では Decoration とか呼ばれているかもしれません.

言語自体にいくつかの Attribute が定義されています[^attr].
`@@deprecated`, `@@@warning`, `@inline` などはそうで, よく見るかもしれません.
知らない Attribute は無視されます.

```ocaml
42 [@inlined]

let x = 42 [@@deprecated "Don't use x"]

[@@@warning -32]
```

### Extension Point
extension Point は Attribute と似たような見た目をしていて,
`[%...]` みたく Attribute の `@` が `%` に変わっただけです.
ただし Extension Point はコードの一部を穴あきにしたような感じで置かれ,
あくまで補助的な Attribute とは本質的に異なります.
PPX で処理しないと当然エラーとなります.

Extension Point にはいくつか中置記法が用意されていて,
いくつかの式に対しては `[]` 無しで記述できます
(というか中置の方が一般的な印象です).
例えば次の二行は等価です.
```ocaml
let y = [%foo let x = 2 in x + 1]
let y = let%foo x = 2 in x + 1
```


### Pre-Processor eXtension (PPX)
上記の Attribute, Extension Point などを型検査前にプリプロセスで処理して置換することができます.
この変換する関数が ppx と呼ばれてます.
ppx はコードを構文解析した後の AST が与えられて AST を返す関数です.
型付け前なので型情報は持っていません[^comp].

AST を組み換えられるので結構色々できる気がします.
よくあるのはコード生成, コード展開, マクロ辺りでしょうか.
```ocaml
type point3d = float * float * float
[@@deriving show] (* show 関数を自動定義 *)

match%lwt x with ... ==> [%lwt match x with ...] (* exptention point *)
==> Lwt.bind x (fun x -> match x with ...) (* モナドで持ち上がった x を展開 *)
```

他にも, ppx のために
- `[g-z|G-Z]`の1文字で終わる int か float のリテラル
- `#` で始まってもう1文字以上 `#` を含む中置演算子
- `(? ∣ ~ ∣ !)`で始まって少なくとも1文字以上 `#` を含む前置演算子

が予約されています(演算子はどう使うんだろう...?).
```ocaml
123456z ==> Z.of_int 123456 (* 多倍長整数へ変換 *)
```

もちろん各々の ppx は最初から入っているわけではありません.
お好きな ppx を書くか探してきて dune の設定ファイルに書いてください.
どんな ppx があるのかは,
<http://ocamlverse.net/content/metaprogramming.html> 辺りを見るといいかもしれません.


## Substitution inside a signature
そもそも `module名 with type t = u` として型同士の制約を追記できるのをどれくらいの人が知っているんでしょうか.
```ocaml
module type Monad = sig
    type 'a t (* abstract type *)
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end
module type OptionMonad : Monad with type 'a t = 'a option
======
module type OptionMonad = sig
    type 'a t = 'a option
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
```
例えば上のように, Monad モジュール型を定義し, `type 'a t` を抽象型としおきます.
そしてモナドの一種として OptionMonad 型を具体的に定義したくなったとき,
`Monad with type 'a t = 'a option` とすると `'a Monad.t` を `'a option` へ具体化して定義することができます.

さらに, `=` だったところを `:=` とすると, 型を代入することができます(!?).
```ocaml
module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end
module type OptionMonad : Monad with type 'a t := 'a option
======
module type OptionMonad = sig
    val return : 'a -> 'a option
    val bind : 'a option -> ('a -> 'b option) -> 'b option
  end
```
制約の追加が代入になったことで, 下の OptionMonad の型定義がなくなり,
`'a t` が `'a option` に全て置き換わってますね.

加えて, 最近 module type を module type で代入できるようになったらしいです(??).

## GADT
ヴァリアント型は ADT (代数的データ型)と呼ばれていたりしますが,
それを拡張したのが GADT です.
ADT の型検査を強くした感じですが,
長さがちょうど n のリストを定義できますというと強くなったのが実感できるでしょうか.
```ocaml
type zero = unit
type 'a succ = unit -> 'a

type (_, _) n_list =
| Nil : ('a, zero) n_list
| Cons : ('a * ('a, 'b) n_list) -> ('a, 'b succ) n_list
=========
Cons (0, Cons (1, Nil)) : (int, zero succ succ) n_list
```

ユースケースとしては, 言語の表現で使われることが多い(くらいしかない?)でしょうか.

GADTの難しさはこの辺[^gadt]で紹介されていた気がしますが,
ここでは一つ謎記法を紹介します.
```ocaml
type _ expr =
| Int : int -> int expr
| Add : (int -> int -> int) expr
| App : ('a -> 'b) expr * 'a expr -> 'b expr

let rec eval : type t. t expr -> t = function
| Int n -> n
| Add -> (+)
| App (f, x) -> eval f (eval x)
```
紹介したいのは `type t. t expr -> t` ですが, これは何なんでしょうか.
その正体に踏み込むには型注釈の記法について2つ紹介しなければなりません.

### Explicit Polymorphic Annotations
```ocaml
type 'a nested = List of 'a list | Nested of 'a list nested
let rec depth = function
    | List _ -> 1
    | Nested n -> 1 + depth n
```
まずこのようなコードを考えます[^epa]. 実はこの `depth` は型検査に失敗します.
コンパイラは `depth` の引数の型を `'a nested` に推論しますが,
再帰で呼ばれている `depth` に与えられる `n` は `'a list nested` で,
型が合わずにエラーとなってしまいます.

`'a nested` 型なのは正しいのですが,
再帰で呼ばれている `depth` の引数の型も同じ `'a nested` であるのがまずくて,
`'b nested` で一般化しないといけません.
そして, 次のように型注釈を付けると `'a` を多相化することができます.
これが Explicit Polymorphic Annotation です.
```ocaml
let rec depth : 'a. 'a nested -> int = function ...
```

### Locally Abstract Type
通常型変数名には `'a` のようなシングルクォート付きの適当な英小文字列を使いますが,
引数を書く場所に `(type a)` と書いて, 型変数名を与えることもできます.
```ocaml
let f (type a) (x : a) = ...
fun (type b) (x : b) -> ...
```

GADT の他にも let 内で module を束縛したりする場合に必要となります[^lat].
例えば次のようにローカルに module S を束縛する場合,
`type t` として与える型をどこかしらから持ってくる必要があります.
```ocaml
let sort_uniq (type s) (cmp : s -> s -> int) =
  let module S = Set.Make(struct type t = s let compare = cmp end) in
  fun l -> S.elements (List.fold_right S.add l S.empty)
```

### 結局
`type t. t expr -> t` は何なのかという話に戻りますが,
結論をいうとこれは Explicit Polymorphic Annotation と Locally Abstract Type を組み合わせた記法です.
単に足しただけな訳ではないですが, 以下と同じなようです.
```ocaml
let rec eval : 'a. 'a expr -> 'a =
  fun (type a) (x : a expr) ->
  (match x with ... : a)
```
再帰している `expr` の引数の型が異なるため Explicit Polymorphic Annotation が必要なのは分かると思います.
Locally Abstract Type が必要なのは型推論器の都合で,
(詳細まではよく分かりませんが) 自由な型変数がない場合はパターンマッチの型推論がうまくいくようです[^gadt2].

## おわりに
色々踏み込んだ紹介しましたが, functor や Object に全然触れられていない辺りまだまだ奥が深いですね.
最近 5.0 もリリースされるとかでまだまだ進化しそうです.

## Footnote
[^varianceAnnot]: このへん <https://blog.janestreet.com/a-and-a/> を読んで訳した感じです
[^attr]: <https://v2.ocaml.org/manual/attributes.html>
[^ppx]: <https://dailambda.jp/slides/2021-04-09-ppx.html\#/what-is-ppx-1>
[^payload]: 指定すれば式だけではなく, 型, パターンなどもいけます
[^comp]: PPX で型情報を使うために, 無理やり型付けして型情報付き AST を ppx に渡すようにする研究があるとか無いとか
[^gadt]: <https://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2011-show.pdf>
[^epa]: <https://v2.ocaml.org/manual/polymorphism.html> より引用
[^lat]: <https://v2.ocaml.org/manual/locallyabstract.html> より引用
[^gadt2]: <https://v2.ocaml.org/manual/gadts-tutorial.html>
