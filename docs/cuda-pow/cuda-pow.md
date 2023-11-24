# Proof of work を GPU に計算させたら確かに CPU より早かった

CUDA の備忘録です.

## PoW
元祖 blockchain であるところの bitcoin では,
p2p ネットワーク上において有効なチェーンがどれなのかを合意するアルゴリズムとして Proof of Work を採用しています.
PoW ではあるブロックの次のブロックの条件として

`hash(前ブロックの hash, ブロックが含むトランザクションデータの hash, nonse)` $< 2^{256-D}$

を課します. ここで D は難易度, nonse は任意のバイト列です.
$< 2^{256-D}$ は, hash 値の先頭 D 桁が 0 であると言い換えることもできます.

この条件を満たす hash を見つける効率的な方法は現状 nonse のブルートフォースくらいしかないと言われています.

bitcoin ではこのブルートフォースによる hash の探索を block を作るのに必要な仕事と課し,
block の偽造を難しくする仕組みとして導入しています.
早い話が, 世界中の bitcoin のマイナーが費やした計算資源を超える量の計算資源を投入しなければ
ブロックのすり替えはできないというわけですね.
もちろん誰も hash 探索をしなければ成り立たないので,
世界中のマイナーには計算資源を費やした対価としてトークンを付与しています.
このトークンが bitcoin ですね.

逆に(というか学術的に)言うと, bitcoin は分散したデータベースを,
トークンというインセンティブによって成り立つ PoW を導入することで構築したシステムと言えます.

## CPU マイニング
話を戻します.
どうもどこかの学校の課題として, `hash(学生番号, nonse)` の先頭が 6 つ以上 0 であるような nonse
を探索して nonse を提出せよ という課題が出たらしいです.
$2^6$ 程度探索すればまぁ見つかるでしょうという感じの難易度で,
まぁ手で探索しても事足りてしまうんですが,
今回はせっかくなので計算機の力を借りてみます.

これは rust 実装です.
<https://gist.github.com/taiseiKMC/a3c148636cdb699ce6a8ef727ce29623>

有効な nonse が `[0-9]+` の string 文字列という制約でちょっと状況は違いますが,
やることは同じでひたすら sha256 を結合する nonse を変えつつ探索するループを回すだけです.
手元の 11 世代 i7 マシンだと 3,000,000 hash/sec $= 3 * 10^6$ くらいでした[^simd].
1 コアで 1 日くらい回して D=36 まで見つけたような覚えがあります.

ちなみに 8 コア全部使うと 9,000,000 hash/sec $= 9 * 10^6$ くらいまで上がりました.

## GPU マイニング
CPU マイニングは並列処理が得意な GPU マイニングに比べて遅いことで知られているので,
GPU マイニングを試しました.

windows マシンで手持ちの gtx1600s を動かすのに
- geforce のドライバ
- CUDA toolkit
- cudnn

辺りを確かインストールしたんだったんだと思います[^new-ssd].

### CUDA
CUDA は nvidia 製 GPU で動かすプログラムを c++ を拡張した文法で記述できる言語です.
CPU と GPU 間でデータをやりとりして, GPU でどう並列化して計算するかみたいなことを書けるよう拡張してます.
詳しくは [nvidia のスライド](https://www.nvidia.co.jp/docs/IO/59373/VolumeI.pdf) を見ると良いんですかね.

- 後で見つけましたが,
  - [東大の講義資料](https://www.cc.u-tokyo.ac.jp/public/VOL12/No2/201003gpgpu.pdf)
  - [英語公式プログラミングガイド](https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html)

  の方がいいですね

まず関数の定義の前に関数修飾子が書けます.

|関数修飾子||
|:-:|:-:|
|`__global__`|ホスト(CPU)から呼び出せて GPU 上で動く void 関数|
|`__device__`|別の GPU 関数から呼び出せて GPU 上で動く関数|
|`__host__`|CPU から呼び出せて CPU 上で動く関数 (GPU関数をCPUでも動かしたい場合に2つ目につける)|

GPU で動く関数は,
`dim3` 型の `blockIdx`, `blockDim`, `threadIdx` 等 GPU に特有のビルトイン変数にアクセスでき,
これを使ってスレッド毎に違う計算をさせます.

`__global__` な関数をホストから呼び出す際は
`<<<n,m>>>` を関数名の後に付けてグリッドサイズ `n` (= 1グリッド中に存在するブロック数), ブロックサイズ `m` (= 1ブロック中に存在するスレッド数) を明示して呼び出します[^grid].
グリッドサイズとブロックサイズは cuda のサンプルにある deviceQuery を実行すると最大数が表示されるので[^cudaprop],
それを参考に決めると良いらしいです.
色々動かしてみたところ, 別に大きくすればするほど速くなるというものでもない気がします.

GPU のメモリと CPU のメモリは独立していて,
専用の関数でやり取りします.

|関数||
|:-:|:-:|
|`cudaSetDevice(int)`| 使用する GPU の ID を指定する |
|`cudaMalloc(void ** pointer, size_t nbytes)`| GPU 上のメモリを割り当てる |
|`cudaMemset(void * pointer int value size t count)`, `cudaMemset(void pointer, int value, size_t count)`| GPU 上のメモリに値を書き込む |
|`cudaMemcpy(void *dst, void *src, size_t nbytes, enum cudaMemcpyKind direction)`| CPU と GPU のメモリ上のデータを転送する. 方向は direction で指定 |
|`cudaFree(void* pointer)`| GPU 上のメモリを開放する |

もちろんこれ以外にも[公式](https://docs.nvidia.com/cuda/cuda-runtime-api/index.html)にたくさん api があります.

ホスト側の処理には古くない c++ が書けますが,
GPU カーネル側では主要な c++ のライブラリが実装されてないのか,
基本的には c 言語の低レベルな記述をすることになるっぽいです.
例えば vector やスマートポインタはだめでした.
生ポインタ辛い〜

### 実装
C++ の sha256 ライブラリを呼び出し... と言いたいところですが,
CPU 上で sha256 を呼び出しても意味がなく,
GPU 上で実装できるよう CUDA で実装し直さないといけません.
どうもそういうライブラリが提供されているわけでもなさそうなので,
今回は有志の CUDA 実装をコピーして改造する感じでやってます.

<https://gist.github.com/taiseiKMC/43e241de158a794289f20c6f0772a722>

回してみたところ 300,000,000 hash/sec $= 3 * 10^8$ くらい出ました(release ビルドを忘れずに).
数時間くらい回して D=42 くらいまで見つけてます.

## 比較とまとめ
フルでコアを使うと CPU 側も結構な速度出てますが,
GPU 側では桁違いのハッシュレートが出ました.
一般に bitcoin の採掘では 200 倍位差が出るらしいです[^bc-mining].

ちなみにこれを書いているときの bitcoin の最新 block の hash は
`00000000000000000001dd6165493dbaf1a1f0b82972044634422cf7a0d58d7e` で,
D=79 です.
実際 bitcoin　全体で凡そ `470,000,000,000,000,000,000 h/s` $= 4.7 * 10^{20}$ 出ているようです[^hr].
世界中でとんでもない量の計算資源が使われてますねぇ.

CUDA に関しては低レベルで書きにくいし,
ライブラリも GPU で動かすことを考えると既存のものは使えないしで,
これを直接どうこうするのは大変ですね.
特にモダンな c++ ではまず生で扱わない配列やポインタを,
kernel 側では触らないといけないし,
その接続部分では生に変換してやらないといけないので苦労します.

後, VisualStudio の intellisense がエラーをうまく表示してくれない気がします.
Windows でやるなってことかもしれません.

# Footnote
[^simd]: 雑に simd を入れると速くなったかもしれない
[^new-ssd]: ドライバを入れるにあたって OS の入っている ssd の容量が足りず, 新しい ssd を購入して `dd` でクローンを作って換装しました
[^grid]: 正整数でもいけますが, 一般に三次元で, `dim3`　が使われているのも見ます
[^cudaprop]: `cudaGetDeviceProperties(cudaDeviceProp*,int)` でも取得できます
[^bc-mining]: <https://wikiwiki.jp/mc-user-wiki/%E3%83%9E%E3%82%A4%E3%83%8B%E3%83%B3%E3%82%B0/%E3%80%90CPU%E3%83%9E%E3%82%A4%E3%83%8B%E3%83%B3%E3%82%B0%E3%80%91>
[^hr]: <https://www.blockchain.com/explorer/charts/hash-rate>
