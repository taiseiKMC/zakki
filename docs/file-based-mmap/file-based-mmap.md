# Mmap の pageout 挙動調査

## Mmap について
mmap は POSIX にもあるシステムコールです.
mmap を通じて開いたファイルは仮想メモリ空間上に mapping され,
メモリ空間を直接編集することでファイルに変更を加えたりできます.

mmap は何らかのファイルを開く必要があるわけではなく,
裏付けとなるファイルがあるかどうかで区別されます.
- Anonymous mapping
    - 何らかのファイルを開いている訳ではない
    - 主にプロセスが使うメモリ ヒープやスタックを含む
- File-backed mapping
    - 実ファイルをメモリ空間に mapping したもの

malloc で大きいメモリ領域を確保する際, 内部的に mmap で anonymous mapping が作られる場合があるようです[^malloc].

## 物理メモリとの対応
仮想メモリに mapping された領域は全て物理メモリ上に乗っているとは限らず,
カーネルの paging という仕組みにより管理され, [^virtual-memory]
必要に応じて読み込まれるようになっています.
具体的なメモリ管理の仕方はカーネル依存ですが,
一般的に物理メモリが枯渇するまでは page in だけ発生し、
枯渇してくると解放可能なページを page out するようになっています.
(解放する順序は疑似LRU的なので決定されてそうです)
File-backed mapping はファイルに書き込みをすればよいですが,
Anonymous mapping は書き込むファイルが無いため,
swap 領域に page out することになります[^paging].

page out できなくなった場合は OOM-killer を実行します.
この辺りは linux の場合 kswapd が監視しているようです.


## 実験
これを踏まえて次のようなプログラムを実行し, メモリ使用量を計測しました.
1. まず mmap で実メモリサイズ以上のファイルを全部読み込む
    - 読むだけ 保存はしない
2. 大量のデータを allocate する
    - このとき mmap で開いたファイルが GC されないよう気をつける

<iframe src="./graph.pdf" width="100%" height="320px">
</iframe>

- `pmap rss total` が pmap によって表示された RSS の値(大体使用メモリ量)
- `pmap sum of shared rss` が実メモリサイズ以上のファイルを全部読み込むことで使用したメモリ量
    - shared 属性の mapping はプログラム中で shared 属性をつけて開いたファイルだけなので, これの RSS だけ数えるとこのファイルが使うメモリ量がわかるはず

25000ms よりちょっと前の辺りで 2 の実行をしていますね.
最後は kill されました.

- 一定以上のメモリを専有した段階で File-backed mapping によるメモリ使用量は頭打ちになる
    - カーネルが page out を実行するようになったからと思われる
    - 枯渇はしないので OOM-kill はされない
- alloc によって確保されたメモリが増えるにつれ file-backed な mmap で使うメモリ量は単調減少していく
- file-backed なファイルのメモリを page out できなるなってくると合計使用メモリ量が増え始める
- alloc によるメモリ使用量が限界になると OOM-kill される

というのがグラフから分かります.

## pmap, smaps
仮想メモリ上の各 mapping を見るには `pmap <pid>` というコマンドと, `/proc/<pid>/smaps` を読むという方法があります. pmap の情報は smaps から取得した情報の一部です.

`pmaps -x` に先程の実験プログラムの pid を渡したある時点の結果です.
```
695564:   ./a.out tempfile
Address           Kbytes     RSS   Dirty Mode  Mapping
000055f3e745d000     352       4       0 r---- a.out
000055f3e74b5000     420     416       0 r-x-- a.out
000055f3e751e000     128      12       0 r---- a.out
000055f3e753f000       4       4       4 r---- a.out
000055f3e7540000     228     208     208 rw--- a.out
000055f3e7579000      36      16      16 rw---   [ anon ]
000055f3e92f1000     452     116     116 rw---   [ anon ]
00007f3f92314000     260       4       4 rw---   [ anon ]
00007f3f92355000 20480000 5076052       0 rw-s- tempfile
00007f4474355000    3844    2088    2088 rw---   [ anon ]
00007f4474716000     148     148       0 r---- libc-2.31.so
00007f447473b000    1504    1164       0 r-x-- libc-2.31.so
00007f44748b3000     296     172       0 r---- libc-2.31.so
00007f44748fd000       4       0       0 ----- libc-2.31.so
00007f44748fe000      12      12      12 r---- libc-2.31.so
00007f4474901000      12      12      12 rw--- libc-2.31.so
00007f4474904000      16      16      16 rw---   [ anon ]
00007f4474908000       4       4       0 r---- libdl-2.31.so
00007f4474909000       8       8       0 r-x-- libdl-2.31.so
00007f447490b000       4       0       0 r---- libdl-2.31.so
00007f447490c000       4       4       4 r---- libdl-2.31.so
00007f447490d000       4       4       4 rw--- libdl-2.31.so
00007f447490e000      60      52       0 r---- libm-2.31.so
00007f447491d000     668     248       0 r-x-- libm-2.31.so
00007f44749c4000     604       0       0 r---- libm-2.31.so
00007f4474a5b000       4       4       4 r---- libm-2.31.so
00007f4474a5c000       4       4       4 rw--- libm-2.31.so
00007f4474a5d000       8       8       8 rw---   [ anon ]
00007f4474a74000       4       4       0 r---- ld-2.31.so
00007f4474a75000     140     140       0 r-x-- ld-2.31.so
00007f4474a98000      32      32       0 r---- ld-2.31.so
00007f4474aa1000       4       4       4 r---- ld-2.31.so
00007f4474aa2000       4       4       4 rw--- ld-2.31.so
00007f4474aa3000       4       4       4 rw---   [ anon ]
00007ffccfa37000     132      16      16 rw---   [ stack ]
00007ffccfaba000      16       0       0 r----   [ anon ]
00007ffccfabe000       8       4       0 r-x--   [ anon ]
ffffffffff600000       4       0       0 --x--   [ anon ]
---------------- ------- ------- ------- 
total kB         20489436 5080988    2528
```

Address は仮想メモリのアドレスで, `Address` から `Address + KBytes` の領域が mapping されています.
実際にメモリ上に乗っているのは RSS の量だけです.
実行環境では page_size は 4KB なので, RSS も Address も 4KB の倍数になっています.
Dirty は mapping 中で変更があった, つまり page out する際に書き込みをする必要のある量です.
Mode は権限です. rwx に加えて, shared (他のプロセスと実メモリを共有するかどうか) と private (Copy-on-write) の属性があります.
Mapping は開いているファイルです.
共有ライブラリやバイナリが file-backed mapping で開かれている様子や,
stack などのメモリ領域が anonymous mapping で開かれているのが分かります.

## footnote
[^malloc]: https://www.valinux.co.jp/technologylibrary/document/linux/malloc0001/

[^virtual-memory]: https://ja.wikipedia.org/wiki/%E4%BB%AE%E6%83%B3%E8%A8%98%E6%86%B6

[^paging]: https://www.kimullaa.com/entry/2019/12/01/143242#%E3%82%B9%E3%83%AF%E3%83%83%E3%83%97, https://blog.a-know.me/entry/2017/07/15/131555, https://tombo2.hatenablog.com/entry/2016/12/30/233151, https://wiki.bit-hive.com/linuxkernelmemo/pg/Swap%20-%20%E3%83%9A%E3%83%BC%E3%82%B8%E3%82%A2%E3%82%A6%E3%83%88, https://www.kernel.org/doc/gorman/html/understand/understand013.html#toc71
