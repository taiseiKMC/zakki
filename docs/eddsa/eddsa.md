# EdDSA

前回の ECDSA に関連して EdDSA (Edwards-curve Digital Signature Algorithm) について見てみました.

EdDSA に関しては <https://ed25519.cr.yp.to/> に情報源がまとまっています.

## 構造
EdDSA では twisted Edwards 曲線と呼ばれる楕円曲線上での演算を基に定義します.

$$
ax^2 + y^2 = 1 + dx^2y^2
$$

ECDSA でも楕円曲線を使用していましたが, これは(短縮) Weierstrass 型の楕円曲線です.

$$
Y^2 = X^3 + AX + B
$$

これらは Montgomery 型楕円曲線 $\beta v^2 = u^3 + \alpha u^2 + u$ を経由して次のように変換できます

$$
u = \frac{y+1}{y-1}\\
v = \frac{(y+1)}{x(y-1)}\\
\alpha = \frac{2(a+d)}{a-d}\\
\beta = \frac{4}{a-d}\\
\\
X = u-\frac{\alpha}{3}\\
Y = \sqrt{\beta} v\\
A = 1 - \frac{\alpha^2}{3}\\
B = \frac{2\alpha^3}{27}-\frac{\alpha}{3}
$$

($a-d<0$ のときは短縮 Weierstrass 型への変換が定義できませんが, 一般 Weierstrass 型なら定まるそうです)

何が言いたいのかというと, Twisted Edwards 曲線は Weierstrass 型楕円曲線から変換が定まる[^qiita]ので, ECDSA のときと同様に
加算を定義できるということです.

ECDSA の場合の $P$ と $Q$ の加算は, $P$ と $Q$ の 2 点を通る直線と楕円曲線の($P$ と $Q$ でない)交点を R として, $P + Q = -R$ ($R$ の $y$ 座標をマイナスにしたもの) でした.
これに対して, EdDSA の場合は $P$ と $Q$ と $(0, -1)$ を通る双曲線のうち, Twisted Edwards 曲線と(P, Q, (0, -1)でない)交点を R として, $P + Q = -R$ ($R$ の **x**座標をマイナスにしたもの)となります.

座標としては, $P=(x_1, y_1), Q=(x_2, y_2)$ として

$$
(x_1, y_1) + (x_2, y_2) = (\frac{x_1y_2 + x_2y_1}{1 + d x_1 x_2 y_1 y_2}, \frac{y_1 y_2 - a x_1 x_2}{1 - d x_1 x_2 y_1 y_2})
$$

です. ECDSA で無限遠点だった零元 $O$ は EdDSA において $(0, 1)$ です.


## Algorithm
### Parameters
[EdDSA for more curves](https://ed25519.cr.yp.to/eddsa-20150704.pdf) にそのまま記載があります.
元論文は [High-speed high-security signatures](https://ed25519.cr.yp.to/ed25519-20110926.pdf) ですが, 多少一般化されています.

* 巨大な素数 $q$. $F_q$ の素となる[^ext]
* $2^{b-1} > q$ である $b$. 公開鍵は b-bit, 署名は 2b-bit となる
* 2b-bit の出力をする hash 関数 $H$
* $c \in \{2,3\}$. 秘密鍵は $2^c$ の倍数となる
* $c \le n \le b$ であるような $n$. 秘密鍵の先頭は n+1-bit 目は 1, それ以上は 0 で, 末尾 c-bit は 0 となる
* 非ゼロの平方数 $a \in F_q$. $q \equiv 1 \mod 4$ の場合は $a = -1$, $q \equiv 3 \mod 4$ の場合は $a = 1$ が推奨されるそう
* 非平方数 $d \in F_q$. $d \notin \{0, -1\}$ である
* ベースポイント $B \in E = \{ (x, y) \in F_q \times F_q \| a x^2 + y^2 = 1 + dx^2y^2 \}$.
  * ちなみに $O = (0, 1) \in E$
* $lB = 0 \land 2^c l = \#E$ であるような奇素数 $l$
* メッセージをhash化する prehash 関数　$H'$
  * hash関数が$H$, prehash 関数が $H'$ であるような EdDSA を H'-EdDSA-H というそう
  * 元論文では H' は id だった. この場合, PureEdDSA という


### 公開鍵
b-bit の秘密鍵 k に対して,

$$
H(k)=(h_0, ..., h_{2b-1})\\
a=2^n + \sum_{c \le i \lt n} 2^i h_i \\
A = aB
$$

としたとき, A が公開鍵です.

### 署名
メッセージ M に対する署名は

$$
r = H(h_b, ..., h_{2b-1}, H'(M))\\
R = rB\\
s = (r + H(R, A, H'(M))a) \mod l
$$

としたときの, 2b-bit の $(R,s)$です. S は $\mod l$している都合で末尾 c bitは 0 になります.

### 検証
署名の検証は,

$$sB = R + H(R, A, H'(M))A$$

であれば成功です. これは

$$
sB = (r + H(R, A, H'(M))a + nl)B\\
= rB + H(R, A, H'(M))aB + nlB\\
= R + H(R, A, H'(M))A
$$

なので署名が正しければ成り立ちます.
$\#E = 2^c l$ なため $2^c$ 倍してもセキュリティレベルが落ちないそうで, プロトコルとしては以下が検証に使われます.

$$2^c sB = 2^c R + 2^c H(R, A, H'(M))A$$

## Example
### Ed25519
EdDSA のインスタンスとしては Ed25519 が有名で, これは Pure-EdDSA-SHA512 です.

$$
q = 2^{255} - 19\\
b = 256\\
c = 3\\
n = 254\\
a = -1\\
d = -\frac{121665}{121666}\\
B_y = \frac{4}{5}\\
l = 2^{252} + 27742317777372353535851937790883648493
$$

H は SHA512, H' は id です. $B_x$ は正(奇数を負, 偶数を正と定義します)の方を採用します.

H' を SHA512 とした場合, SHA-512-Ed25519-SHA-512 となり, 一応 Ed25519ph として定義されているそうです.

### Ed448
Ed25519 以外の EdDSA に馴染みはないのですが, Ed448 も  EdDSA としてはメジャーなようです. Ed448-SHAKE-256 は

$$
q = 2^{448} -2^{224} - 1\\
b = 456\\
c = 2\\
n = 448\\
a = 1\\
d = -39081\\
B_y = 19\\
l = 2^{446} - 13818066809895115352007386748515426880336692474882178609894547503885
$$

H は SHAKE-256, H' は id です. 256 とありますが, 実際には出力ビット数は可変で, Ed448 の場合は 512 bit です. 一応同様に Ed448ph が定義されます.

## Implementation
今回は (Pure)Ed25519 参考実装 <https://ed25519.cr.yp.to/python/ed25519.py> を読みました.
Ed25519 は高速に計算しやすいようチューニングされているっぽいですが, この実装では理解のために愚直に実装されています.
署名と検証が素直に実装されているのであまり注釈するところはないのですが,
xrecover の部分だけよくわからなかったので注釈します.

```python
I = expmod(2,(q-1)/4,q)

def xrecover(y):
  xx = (y*y-1) * inv(d*y*y+1)
  x = expmod(xx,(q+3)/8,q)
  if (x*x - xx) % q != 0: x = (x*I) % q
  if x % 2 != 0: x = q-x
  return x
```

この部分ではオイラーの規準(Euler's criterion):
$$
p^{\frac{q-1}{2}} \equiv 
\begin{cases}
1 \mod q & \text{when p is square}\\
-1 \mod q & \text{otherwise}
\end{cases}
$$
が利用されています.

2 は平方数でないので, $2^\frac{q-1}{2} \equiv -1 \mod q$ です.
つまり,$I = 2^\frac{q-1}{4} \equiv \sqrt{2^\frac{q-1}{2}} \equiv \sqrt{-1} \mod q$ です.
ちなみに $q \equiv 5\mod 8$ なので $\frac{q-1}{4}$ は整数です.

さて xrecover の方を見ていきます. ちょっとわかりにくいので, $z = \text{xx}$ と置き直します.
$z= \frac{y^2 -1}{dy^2 + 1}$ は元の方程式
$ax^2 + y^2 = 1 + dx^2y^2$ ($a = -1$) を変形したものです.
$z$ は $x$ の自乗なので平方数です. つまり, $z^\frac{q-1}{2} \equiv 1 \mod q$ です.
変形を重ねて,

$$
\begin{aligned}
z^\frac{q-1}{4} \equiv z^{\frac{q+3}{4} - 1} \equiv \pm 1 \mod q\\
\leftrightarrow z^\frac{q+3}{4} = \pm z \mod q\\
\leftrightarrow z^\frac{q+3}{8} = \pm \sqrt(\pm z) \mod q
\end{aligned}
$$

つまり最初に x に代入している時点では x は確定してなくて, 真の $x$ は $\text{x}, -\text{x}, \text{x}\sqrt{-1}, -\text{x}\sqrt{-1}$ の 4 通り[^cand]候補がある状態です. 次の2行では, この候補を絞り込んでいます.

このアルゴリズムは $q \equiv 5 \mod 8$ を満たす($\frac{q+3}{8}$ が整数になる) Ed25519 だから適用できる手法で,
$q \equiv 3 \mod 4$ である Ed448 では別のアプローチが必要です.
($(z^\frac{q+1}{4})^2 \equiv z^\frac{q+1}{2} \equiv z \times z^\frac{q-1}{2} \equiv z \mod q \leftrightarrow z^\frac{q+1}{4} \equiv \pm \sqrt{z} \mod q$
で, Ed25519 のときよりシンプルになります)

# Footnote
[^qiita]: <https://qiita.com/angel_p_57/items/a1dc4e9c0b18a23c5242>
[^ext]: 元論文では $q \equiv 1 \mod 4$ という条件があったが, 拡張で外れた
[^cand]: x, -x はコンパチなので, 正規化しなくて良いならどちらでもよいという理解
