# ShellScript(bash) 文法まとめ
読みにくいし環境依存な要素が多いし遅いしでガッツリ使いたくないなーとは思いつつも,
なんだかんだ使う機会があるshellscript.
書こうとする度にちゃんとした文法が思い出せずにググっては香ばしいサイトを踏んで渋い思いをすることが多かったので,
`man bash` を読んで備忘録がてら文法をまとめて置いておく(`man bash` の劣化コピーのような感じもするので, 多分他の人は本家を読んだほうが良い).

とりあえず linux 環境で自分が動かせれば良いという目標なので, bash で動かせれば良しとする.

## 文法
UNIXコマンド説明風に, メタ変数は大文字, オプションは `[]` で囲み, OR は `|` で区切り, 任意長の繰り返しは `...` で表現する.
`|` や `[]` を記号として使う場合の表記が被っているが, 空気を読んでということで...

### コマンド
```shell
[NAME0=VALUE0 ...] WORD0 [WORD1 ...] [REDIRECTION]
```
1コマンドは, コマンド名 WORD0 と引数の列 WORD1.. からなる.
変数代入の列が WORD0 の前に来ても良い.
REDIRECTION は意味はともかく末尾でなくても書ける.

### Pipeline
```shell
[time [-p]] [ ! ] CMD0 [ [| ⎪ |&] CMD1 ... ]
```
`|` で繋げたコマンドは連続で実行され, CMD0 の出力が CMD1 の入力として渡される.
`|&` は `2>&1 |` の略記.
`!` をつけると exit status が反転する.

`/usr/bin/time` と `time` で挙動が違うなーとは思っていたけれど,
bash において `time` はコマンドじゃなくて予約語だったかららしい.

### List
コマンドは `&&`, `||`, `;`, `&`, newline のいずれかで連ねることができる

| 記号 | 意味 |
|:----:|:---: |
| `CMD0 && CMD1` | CMD0 が正常終了したときのみ CMD1 を実行する |
| `CMD0 \|\| CMD1` | CMD0 が異常終了したときのみ CMD1 を実行する |
| `CMD0 & CMD1` | CMD0 をバックグラウンド実行し, CMD1 を実行する |
| `CMD0 ; CMD1` | CMD0 を実行し, CMD1 を実行する |
| `CMD0` newline `CMD!` | `;` と同じ |

結合順序は `&&` = `||` > `;` = `&`

### Expression
C言語っぽい感じで算術式は書ける(省略).

条件式は `[[]]` で囲む(bash拡張).

|||
|:-:|:-:|
|`ARG0 && ARG0`| 論理積 |
|`ARG0 \|\| ARG1`| 論理和 |
|`-a FILE`| FILE が存在するか |
|`-e FILE`| FILE が存在するか |
|`-b FILE`| FILE が存在し, かつブロック特殊ファイルであるか |
|`-c FILE`| FILE が存在し, かつキャラクター特殊ファイルであるか |
|`-d FILE`| FILE が存在し, かつディレクトリであるか |
|`-f FILE`| FILE が存在し, かつ通常ファイルであるか |
|`-g FILE`| FILE が存在し, かつ set-group-id されているか |
|`-u FILE`| FILE が存在し,  かつ set-user-id ビットが設定されているか |
|`-k FILE`| FILE が存在し, かつ sticky ビットが設定されているか |
|`-h FILE`| FILE が存在し, かつシンボリックリンクであるか |
|`-L FILE`| FILE が存在し, かつシンボリックリンクであるか |
|`-p FILE`| FILE が存在し, かつ名前付きパイプ (FIFO) であるか |
|`-r FILE`| FILE が存在し, かつ読み込み可能であるか |
|`-w FILE`| FILE が存在し, かつ書き込み可能であるか |
|`-x FILE`| FILE が存在し, かつ実行可能であるか |
|`-s FILE`| FILE が存在し, かつそのサイズが 0 より大きいか |
|`-t FD` | ファイル・ディスクリプタ FD がオープンされており, かつ端末を参照しているか |
|`-G FILE`| FILE が存在し, かつ (実行中のシェルの) 実行グループ ID に所有されているか|
|`-O FILE`| FILE が存在し, かつ (実行中のシェルの) 実行ユーザー ID に所有されているか |
|`-N FILE`| FILE が存在し, かつ前回読み込んでから変更されているか |
|`-S FILE`| FILE が存在し, かつソケットであるか |
|`FILE0 -ef FILE1`| FILE0 と FILE1 とで デバイス番号と i-node 番号が同じであるか |
|`FILE0 -nt FILE1`| FILE0 が (変更日時に関して) FILE1 より新しい, または FILE0 が存在するが FILE1 が存在しない |
|`FILE0 -ot FILE1`| FILE0 が FILE1 より古いか, FILE1 が存在するのに FILE0 が存在しない |
|`-o OPTNAME`| シェルオプション OPTNAME が有効かどうか (set の -o オプションを参照) |
|`-v NAME`| シェル変数 NAME がセットされているか (値が代入されているか) |
|`STRING`| STRING の長さが非零であるか |
|`-z STRING`| STRING の長さが 0 であるか |
|`ARG0 -eq ARG1`| ARG0 = ARG1 であるか |
|`ARG0 -ne ARG1`| ARG0 != ARG1 であるか |
|`ARG0 -lt ARG1`| ARG0 < ARG1 であるか |
|`ARG0 -le ARG1`| ARG0 <= ARG1 であるか |
|`ARG0 -gt ARG1`| ARG0 > ARG1 であるか |
|`ARG0 -ge ARG1`| ARG0 >= ARG1 であるか |
|`STRING == PATTERN`| パターンマッチ |
|`STRING != PATTERN`| パターンマッチ失敗 |
|`STRING =~ PATTERN`| 拡張正規表現マッチング |

または `[]` で囲む(POSIX準拠, `test` コマンドに渡される).
こちらはパターンマッチ辺りは無くて, `&&` や `||` は `-a` や `-o` を代わりに使う.

### 複合コマンド

|||
|:-:|:-:|
|`(LIST)`| サブシェルで LIST を実行する |
|`{LIST;}`| 現在のシェルで LIST を実行する |
|`((EXPR))`| 算術式 (letコマンドと同じ) |
|`[[EXPR]]`| 条件式 |

### if 文
```shell
if LIST; then LIST; [ elif LIST; then LIST; ] ... [ else LIST; ] fi
```
if または elif の LIST の exit status が 0 なら then 節の LIST を実行する.

### for 文
```shell
for NAME in WORD ; do LIST ; done
for (( EXPR0 ; EXPR1 ; EXPR2 )) ; do LIST ; done
```
Range-based for と普通の for が書ける.

### while 文
```shell
while LIST0; do LIST1; done
until LIST0; do LIST1; done
```
LIST0 の exit status が 0 の間, LIST1 を繰り返し実行する.
until 文は LIST0 の条件が反転する.
他のループもだが, ビルトインコマンドの `break` と `continue` が使える.
- `break [N]`で N 個のループを抜けられる.
- `continue [N]` で N 個上のループで実行を継続する.

### select 文
```shell
select NAME in WORD ; do LIST ; done
```
WORD 内の要素を選択肢として出力し, 選択された要素を NAME に代入して LIST を実行するループ.
対話的に動く.

### case 文
```shell
case WORD in [ [(] PATTERN [ | PATTERN ] ... ) LIST [;; | ;& | ;;& ] ] ... esac
```
WORD を PATTERN でパターンマッチする.マッチしたら LIST を実行する.
PATTERN は `|` で繋げて複数列挙できる.
LIST の末尾が `;;` の場合は終了し, `;&` の場合は次の LIST を連続して実行し,
`;;&` の場合はパターンマッチを続行する.

### 関数
```shell
FNAME () CCMD [REDIRECTION]
function FNAME [()] CCMD [REDIRECTION]
```
function キーワードの場合において `()` を省略した場合, CCMD は括弧を省略できないが, それ以外の場合は省略できる.
リダイレクトも書ける.

CCMD 中で `local` コマンドを使ってローカル変数の宣言ができる.
`return` コマンドで関数実行を終了する.

### 変数
```shell
NAME=[VALUE]
```
VALUE が指定されなかった場合, null 文字が代入される.
NAME 変数を完全に削除するには `unset` コマンドを使う.
`+=` を書くと変数の属性によって挙動が変わる.
- 整数属性(-i)の場合は算術式展開して加算する
- 文字列属性の場合は文字列を付け加える
- 配列属性(-a)の場合は要素を末尾に加える
- 連想配列属性(-A)の場合はキーと値を加える (普通は偶数個加える)

#### 特殊変数

|||
|:-:|:-:|
| 正整数 `n` | `$1`, `${10}` などでn番目のコマンドライン引数 (positional parameter) へアクセスできる |
| `0` | シェルまたはスクリプトの名前 |
| `_` | シェルまたはスクリプトの絶対パス |
| `*`, `@` | 全ての positional parameter に展開する.`*` の場合, `IFS`変数の最初の文字を区切り文字として並べる.`$@`の場合, `$1 $2 $3...` という感じ.|
| `#` | positional parameter の総数 |
| `?` | 最後に実行したパイプラインのフォアグラウンドプロセスの終了ステータス |
| `-` | シェルのオプション |
| `$` | シェルのプロセスID |
| `!` | 最後に実行したバックグラウンドプロセスの ID |

#### 配列
```shell
NAME=([INDEX0]=VALUE0 [INDEX1]=VALUE1...)
```
- `NAME` が未定義の場合, `NAME[INDEX]=VALUE` とすると配列 `NAME` が生成される
- `declare -A NAME`とすると明示的に配列を生成できる
  - 続けて `=` を書いて値を代入できる
- INDEX が 0 から連番の場合は `[INDEX0]=` の部分を省略して `NAME=(VALUE0 VALUE1...)` と書ける
- 多次元配列とかはできなさそう

`${NAME[@]}` または `${NAME[*]}` で配列の要素を全て展開する.

### 展開
コマンドラインの入力に対してコマンド実行前に展開が行われる.
サブタイトルの展開の他にも以下がある
- ブレース展開 (bash拡張)
  - `{X..Y[..INCR]}` で X から Y までの文字を列挙
- チルダ展開
  - `~` で HOME が展開される
  - `~+` で PWD が展開される
- 単語分割
  - 先行する展開の結果が IFS を含んでいた場合, 分割する
- パス名展開
  - `*`, `?`. `[...]` で記述したパス名を展開する

#### パラメータ展開
`$` 文字があると展開が行われる. 曖昧さ回避のために`{}`で括ることもできる.

|||
|:-:|:-:|
| `${PARAM:-WORD}` | PARAM が未定義または null の場合, WORD が使われる(デフォルト値) |
| `${PARAM:=WORD}` | PARAM が未定義または null の場合, WORD を PARAM に代入して使う |
| `${PARAM:?WORD}` | PARAM が未定義または null の場合, WORD をエラー出力する |
| `${PARAM:+WORD}` | PARAM が未定義または null の場合は WORD, そうでなければ null 文字を使う |
| `${PARAM:OFFSET}` | PARAM の OFFSET 文字目以降を使う |
| `${PARAM:OFFSET:LENGTH}` | PARAM の OFFSET 文字目以降を LENGTH 文字使う |
| `${!PREFIX*}` | PREFIX を接頭辞とする全ての変数名へ展開される |
| `${!PREFIX@}` | PREFIX を接頭辞とする全ての変数名へ展開される |
| `${!PARAM[*]}` | PARAM が配列のとき, 全てのキーへ展開される.配列でない変数の場合は0へ, 未定義の場合は null 文字へ展開される |
| `${!PARAM[@]}` | PARAM が配列のとき, 全てのキーへ展開される.配列でない変数の場合は0へ, 未定義の場合は null 文字へ展開される |
| `${#PARAM}` | PARAM の文字数へ展開される.PARAM が`*`または`@`の場合は引数の数,`*`または`@`が添字になっている配列ならば配列の要素数へ展開される |
| `${PARAM#PATTERN}` | PARAM の前方から PATTERN に一致する部分を取り除く(最短一致). 配列の場合は全ての要素に適用する |
| `${PARAM##PATTERN}` | PARAM の前方から PATTERN に一致する部分を取り除く(最長一致). 配列の場合は全ての要素に適用する |
| `${PARAM%PATTERN}` | PARAM の後方から PATTERN に一致する部分を取り除く(最短一致). 配列の場合は全ての要素に適用する |
| `${PARAM%%PATTERN}` | PARAM の後方から PATTERN に一致する部分を取り除く(最長一致). 配列の場合は全ての要素に適用する |
| `${PARAM/PATTERN/WORD}` | PARAM から PATTERN に最長一致する最初の部分を WORD へ置換する |
| `${PARAM//PATTERN/WORD}` | PARAM から PATTERN に最長一致する全ての部分を WORD へ置換する |
| `${PARAM/#PATTERN/WORD}` | PARAM から PATTERN に最長一致する接頭辞を WORD へ置換する |
| `${PARAM/%PATTERN/WORD}` | PARAM から PATTERN に最長一致する接尾辞を WORD へ置換する |
| `${PARAM^PATTERN}` | PARAM から PATTERN に一致する最初の文字を大文字へ変換する |
| `${PARAM^^PATTERN}` | PARAM から PATTERN に一致する全ての文字を大文字へ変換する |
| `${PARAM,PATTERN}` | PARAM から PATTERN に一致する最初の文字を小文字へ変換する |
| `${PARAM,,PATTERN}` | PARAM から PATTERN に一致する全ての文字を小文字へ変換する |

#### コマンド置換
`$(LIST)` または `` `LIST` `` でコマンドの実行結果へ展開する.
`$(cat FILE)` は `$(< FILE)` と書くと高速になる.

#### 算術式展開
`$((EXPR))` で EXPR の評価結果へ展開する

#### プロセス展開
`<(LIST)` または `>(LIST)` で LIST の評価結果を接続したファイルへ展開する.

```shell:例
diff <( CMD0 ) <( CMD1 )
```

### リダイレクト

|||
|:-:|:-:|
| `[N]<WORD` | ファイル名 WORD を開き, ファイルディスクリプタ N で読み込めるようにする |
| `[N]>WORD` | ファイル名 WORD を開き, ファイルディスクリプタ N で書き込めるようにする |
| `[N]>>WORD` | ファイル WORD へ追加で書き込む |
| `&>WORD`, `>&WORD` | `> WORD 2>&1` と同じ |
| `&>>WORD` | `>> WORD 2>&1` と同じ |
| `<<WORD \n ... \n WORD` | WORD を最初に指定し, WORD が再び単独で現れる行までを標準入力として渡す (Here Documents) |
| `<<<WORD` | WORDを標準入力として渡す (Here Strings) |
| `[N]<&WORD` | 入力ファイルディスクリプタ N を WORD へ複製する. WORDは整数でも良い |
| `[N]<&-` | 入力ファイルディスクリプタ N をクローズする |
| `[N]>&WORD` | 出力ファイルディスクリプタ N を WORD へ複製する |
| `[N]>&-` | 出力ファイルディスクリプタ N をクローズする |
| `[N]<&DIGIT-` | 入力ファイルディスクリプタ DIGIT を N へ変更する |
| `[N]>&DIGIT-` | 出力ファイルディスクリプタ DIGIT を N へ変更する |
| `[N]<>WORD` | 入出力ファイルディスクリプタを N で開く |

## よく使いそうなコマンド
- awk
- sed
- tr
- grep
- sort
- uniq
- seq
- tee
- xargs

## WIT
- Shebang

  ファイルの一行目に書く `#!/bin/sh` みたいなののこと.
  `#!/bin/bash -eu` と思考停止で書いていいと思う.

- bash option

  `set -e` みたくコマンドで後から指定もできる
  - `-e` : エラーが起きたら終了し, 残りのスクリプトを無視する
  - `-u` : 未使用の変数を使った場合エラーになる
    - デフォルトだと空の変数となる
  - `-v` : 読んだスクリプトを出力する
  - `-x` : 実行したコマンドを出力する

- 実行ディレクトリ統一

  相対パスを使っていたりすると shellscript が動かなかったりすることがあるが,
  `cd $(dirname $0)` とすると shellscript のあるディレクトリへ移動してから実行するので
  そういったエラーがなくなる

- [shellcheck](https://github.com/koalaman/shellcheck)

  shellscript の検査器を見つけた.
  スペースの有無等何かと罠が多いので標準搭載しても良いのでは.

- あまりに遅い

  練習がてら AtCoder の問題を解いてみたが,
  $10^5$ が TLE で通らなくて流石に使い物にならない...
