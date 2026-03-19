# ECDSA を実装した

楕円曲線暗号が, 楕円曲線上で定義される演算をもとに計算されることは知っていましたが,
解像度を上げるために実装してみました.
ここでは楕円曲線の前提知識みたいなところはあまり解説しません.

仕様 : <https://www.secg.org/SEC1-Ver-1.0.pdf>

## 楕円曲線上での演算
楕円曲線とは $y^2 = x^3 + ax + b$ で表される曲線です. 楕円曲線暗号では,
$x, y$ は素体 $F_p$ の元で, $P$ は曲線上の点と無限遠点 $O$ から成ります.
$p_0, p_1 \in P$ に対し, 加算 $p_0 + p_1$ は
1. $p_0$ と $p_1$ を結ぶ直線と楕円曲線の($p_0, p_1$ とは異なる)交点
2. $p_0$ と $p_1$ が等しい場合, $p_0$ における楕円曲線の接線と, 楕円曲線の($p_0$ とは異なる)交点
3. 上記で楕円曲線との交点が存在しない場合, 無限遠点 $O$
で定義されます. 無限遠点 $O$ は単位元です.

楕円曲線上の2点を通る直線ともう一つの交点は意外にもシンプルに求まります.
傾きを $l$ として, 2点を通る直線は $y = l(x - x_1) + y_1$ です.
楕円曲線の式からその交点は, $0 = y^2 - (x^3 + ax + b) = (l x - l x_1 + y_1)^2 - (x^3 + ax + b) = (x - x_1)(x - x_2)(x - x_3)$ が成り立ちます.
ここで $x^2$ の係数に注目すると,
$l^2 = x_1 + x_2 + x_3 \leftrightarrow x_3 = l^2 - x_1 - x_2$
となります.

$l$ は 1 のケースだと $\frac{y_2 - y_1}{x_2 - x_1}$ ですね.
2 のケースでは, 接線の傾きは楕円曲線を微分して $y' = (\sqrt{x^3 + a x + b})' = (3x^2 + a) \frac{1}{2 \sqrt(x^3 + ax + b)} = (3x^2 + a) \frac{1}{2 y}$ で求められます.
```python
class EcPoint:
    # Fp は p を法とした整数のクラス
    def __init__(self, x : Fp, y : Fp):
        self.x = x
        self.y = y

    def __add__(self, rhs: EcPoint):
        if isinstance(rhs, ZeroPoint):
            return self
        if self.x == rhs.x:
            if self.y == -rhs.y:
                return ZeroPoint() # case 3
            # case 2
            l = (Fp(3) * self.x ** 2 + a) / (Fp(2) * self.y)
        else:
            # case 1
            l = (rhs.y - self.y) / (rhs.x - self.x)
        
        # Viète's formula
        x3 = l ** 2 - self.x - rhs.x
        y3 = l * (self.x - x3) - self.y
        return EcPoint(x3, y3)

# 無限遠点 O
class ZeroPoint(EcPoint):
    def __init__(self):
        pass
    def __add__(self, rhs):
        return rhs
```
加算が定義できれば乗算も自明です.
この乗算には $q = k p$ を計算するのに比べて, $q$ から $k$ と $p$ を求めることが難しいという性質(離散対数問題)があり,
楕円曲線暗号はこの性質を利用しています.

## 署名と検証
ECDSA では private key $s_k$ を $[1, n)$ の整数, public key $p_k$ を $s_k * G$ で生成します.
ここで基準点 $G$ は $P$ 上の点で, $n$ は $G$ の巡回数です.
ハッシュ値 $h$ に対する署名は, 乱数 $k \in [1, n)$ を生成し, $r = (kG).x$, $s = \frac{h + r s_k}{k}$ で生成します.
検証時は $h, r, s$ を受け取り, $\frac{h G + r p_k}{s}$ の x 座標と r が一致すれば成功です.
($\frac{h G + r p_k}{s} = \frac{k h G + k r p_k}{h + r s_k}= \frac{k h G + k r s_k G}{h + r s_k} = \frac{k G(h + r s_k)}{h + r s_k} = k G = r$)

```python
from hashlib import sha256

def generateKeyPair():
    private_key = Fn(secrets.randbelow(n - 1) + 1)
    public_key = private_key * G
    return private_key, public_key

# Fn は n を法とした整数クラス
def sign(msg : bytes, private_key : Fn):
    k = Fn(secrets.randbelow(n - 1) + 1)
    hash = byteToInt(sha256(msg).digest())
    kg = k * G
    r = Fn(kg.x.v)
    s = (Fn(hash) + private_key * r) / k
    if r == Fn(0) or s == Fn(0):
        # 再生成する
        return sign(msg, private_key)
    return (r, s)

def verify(msg : bytes, signature : tuple[Fn, Fn], public_key : EcPoint):
    r, s = signature
    hash = byteToInt(sha256(msg).digest())
    a = (Fn(hash) / s) * G
    b = (r / s) * public_key
    c = a + b
    return c.x == r
```


## Parameters
<https://www.secg.org/sec2-v2.pdf> に SECG で標準化された楕円曲線暗号で使う推奨パラメーターが記載されています.
例えば secp256k1 のパラメーターは以下です.

```
p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
n = 0XFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
# G = (gx, gy)
gx = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
a = 0
b = 7
```

ちなみに secp256k1 は
* sec : Standards for Efficient Cryptography Group
* p/t : prime/binary field
* 256 : フィールドサイズ
* k/r : Koblitz/random curve[^koblitz]
を表しているのだそうです.

他にも NIST による標準化が知られていますが, その P-256 は secp256r1 と同じです.

## 感想や補足

* 実装していて, $F_p$ と $F_n$ の 2 つの素体が登場します. 考えてみれば当たり前ではあるのですが, $G$ の巡回数と $G$ の座標の法を取り違えないよう注意する必要があります.  
  また, `sign` において `r = Fn(kg.x.v)` の箇所で $F_p$ から $F_n$ への値の変換が挟まるのも直感的でない点です.

* 非常に低確率ですが, ECDSA の仕様としても r または s が 0 の場合は k を選び直して署名を再生成することになっています.  
  (r==0 の場合は, s の計算に private_key を使わないため署名が意味を為さず, s==0 の場合は 0 除算です)

* 署名 $(r, s)$ に対して, 実は $(r, -s)$ も有効な署名になります.  
  ($\frac{h G + r p_k}{-s} = - \frac{h G + r p_k}{s}$ で, 楕円曲線上の点 $P$ と $-P$ は $x$ 座標が等しく, $r$ と一致する)  
  この二種類の署名のうち, $s < \frac{n}{2}$ である方を low-s 値と呼び, bitcoin では署名の改変可能性の排除のために low-s のみを正しい署名として扱っていたりします.

* $k$ はセキュアランダムである必要があり, 使いまわすと private key が漏洩することで知られています.  
  同じ k で署名を生成していたことで秘密鍵が漏洩した事件として PS3 が有名です[^ps3].


## Encoding
鍵は通常 PEM (Privacy Enhanced Mail) や DER (Distinguished Encoding Rules) 形式で出力されます.
PEM は `-----BEGIN XXX-----` と `-----END XXX-----` で囲まれた base64 エンコードの文字列で, デコードした中身は DER と同一です.
ASN.1 (Abstract Syntax Notation One) というデータ構造を定義する汎用的な DSL があり, ASN.1 でシリアライズされたバイナリが DER です[^der].

ECDSA の署名の場合,
```
SequenceTag(30) Length
    IntegerTag(02) Length r
    IntegerTag(02) Length s
```
というフォーマットとしていて, 例えば
```
30 45
  02 21 008360c10bf24e9696a2416882f9232c99c0ca516c610d2c0245370408f6692f54
  02 20 389e7e11f785651b7effde296758188339bc5a59a1629f711efa2fa67294bbdd
```
というバイナリになっています[^integer].

python では pyasn1 ライブラリを使って次のように記述できます.
```python
class EcdsaSignature(Sequence):
    componentType = NamedTypes(
        NamedType('r', Integer()),
        NamedType('s', Integer())
    )

sig, _ = decode(sig, asn1Spec=EcdsaSignature()) # _ は未消費のバイト列
r = sig.getComponentByName('r')
s = sig.getComponentByName('s')
```

## openssl による鍵と署名生成
```shell
# 鍵生成
% openssl ecparam -genkey -name secp256k1 -outform DER -out key.der

# 公開鍵取得
% openssl ec -in key.der -inform DER -pubout -outform DER -out pubkey.der

# 署名生成
% echo "hoge" > message.txt
% openssl dgst -sha256 -sign key.der -keyform DER -out sig.der message.txt
```

# Footnote
[^koblitz]: 楕円曲線が Koblitz Curve だと高速化できるのだとか
[^ps3]: <https://en.wikipedia.org/wiki/PlayStation_3_homebrew>
[^der]: <https://letsencrypt.org/ja/docs/a-warm-welcome-to-asn1-and-der/>
[^integer]: DER の Integer は先頭の bit が1だと負の数扱いとなる. s は low-s 形へ正規化することで 32bytes にできるが, r は 32 bytesだったり33 bytes だったりして DER 形式のデータ長に微妙に差が発生する.
