# gdbの使い方まとめ
gdbのコマンドは、一意に決まるところまで省略できる
- 起動
```
$gdb <file>
$gdb -p <pid>
```

- 実行
    - 引数を与えたり、リダイレクトしたりもできる
    - startで実行するとmain関数に入るときにブレークする
```
gdb$ r
gdb$ start
```
非省略形: run


- 引数をあらかじめセットする
```
gdb$ set args <コマンドライン引数>...
```


- 終了する
```
gdb$ q
```
非省略形: run


- 実行中のプログラムを止める
```
gdb$ k
```
非省略形: kill


- アセンブル言語の記法の変更
```
gdb$ set disassembly-flavor (intel/att)
```


- 逆アセンブル
    - ここのアドレスには*はつけないが、他のアドレスの表記には先頭に*をつける
```
gdb$ disas <関数名/アドレス>
gdb-peda$ pdisas <関数名/アドレス>
gdb-peda$ pdisas <開始アドレス> <終了アドレス>
```
非省略形: disassemble


- ブレークポイントの設置
    - シンボル情報があれば引数に行数を与えることもできる
    - 引数を与えなければ今pcが指しているアドレスにブレークポイントを設置する
```
gdb$ b <関数名/アドレス>
```
非省略形: break


- 設置したブレークポイントの確認
```
gdb$ i b
```
非省略形: info break


- ブレークポイントの削除
    - 引数がないと全てのブレークポイントを消す
```
gdb$ d <ブレークポイントの番号>
```
非省略形: delete


- ブレークポイントの無効化、有効化
```
gdb$ disable <ブレークポイントの番号>
gdb$ enable <ブレークポイントの番号>
gdb$ ignore <ブレークポイントの番号> <無視する回数>
```


- ステップオーバー
    - 繰り返し回数だけステップオーバーを実行する
    - callなどのサブルーチンの中まで入り込まない
    - nは1関数を1ステップ、niは1アセンブリコードを1ステップと扱う
```
gdb$ n <繰り返し回数>
gdb$ ni <繰り返し回数>
```
非省略形: next, nexti


- ステップイン
    - 繰り返し回数だけステップオーバーを実行する
    - callなどのサブルーチンの中まで入り込まない
    - sは1関数を1ステップ、siは1アセンブリコードを1ステップと扱う
```
gdb$ s <繰り返し回数>
gdb$ si <繰り返し回数>
```
非省略形: stepi


- 実行を再開する
```
gdb$ c
```
非省略形: continue


- 次のcallまで実行する
```
gdb-peda$ nextcall
```
非省略形: next, nexti


- 格納された値をみる
```
gdb$ p <変数/レジスタ/アドレス/式>
```
非省略形: print


- アドレスやレジスタの詳しい情報をみる
```
gdb-peda$ xi <変数/レジスタ/アドレス>
```
非省略形: xinfo

- 全てのレジスタの値をみる
```
gdb$ i r
```
非省略形: info registers


- 条件を満たしたときにブレークする
    - 変数/アドレスを指定すると、値が変動するとブレークする
```
gdb$ watch <変数/アドレス>
gdb$ watch <条件式>
```


- 変数や式の型をみる
```
gdb$ what <式>
```
非省略形: whatis


- 読み込みを検出してブレークする
```
gdb$ rw <変数/アドレス>
```
非省略形: rwatch


- 読み書き込みを検出してブレークする
```
gdb$ aw <変数/アドレス>
```
非省略形: awatch


- 値を書き換える
    - pcを書き換えて無理矢理制御を移すこともできる
```
gdb$ set <変数/アドレス/レジスタ>=<値>
```


- メモリの内容を表示する
    - 表示する数は数字
    - フォーマットはbhwgのどれか(byte, halfword, word, giantword)
    - フォーマットはsixのどれか(文字列, 命令, 16進数)
```
gdb$ x/<表示する数><メモリサイズ><表示フォーマット> <先頭アドレス>
```

例:
```
gdb$ x/10i 0x114514
```

- スタックの内容を表示する
```
gdb$ stack
gdb-peda$ tel <長さ>
```
非省略形: telescope

例:
```
gdb$ x/10i 0x114514
```


- セグフォしたとき
    - そのとき実行していた関数が分かる
```
gdb$ i s
```
非省略形: info stack


- コールスタックのバックトレースをする
```
gdb$ bt
```
非省略形: backtrace


- 分からないことがある
```
gdb$ h <コマンド名>
```
非省略形: help

- スクリプト、コマンドを実行 (pedaなど)
```
gdb$ shell <command>
gdb$ source <file名>
```

- fork関連
```
gdb$ set follow-fork-mode parent # default は child
gdb$ set detach-on-fork off # default は on
```

## セキュリティ系拡張
- ASLRの有無を確認
```
gdb-peda$ aslr
```


- セキュリティ機構の有無の確認
```
gdb-peda$ checksec
```


- ROPgadgetを探す
```
gdb-peda$ droprop
```


- ヘッダ情報の取得
```
gdb-peda$ elfheader
gdb-peda$ readelf
```


- マッピングされたアドレスの情報を表示する
```
gdb-peda$ vmmap
```


- 文字列を扱う
    - 長さnの文字列を生成する
    - 長さnの文字列を引数に与える
    - 生成した文字列の何文字目に与えた文字列があるか探索する
```
gdb-peda$ pattern create n
gdb-peda$ pattern arg n
gdb-peda$ pattern offset <文字列>
```