# Unix Pass によるパスワード管理
巷ではパスワード管理ソフトがいくつか出回っているみたいですが,
そんなプロプライエタリ[^comp0]なソフトウェアにパスワードというアイデンティティそのものを任せていいんですか？？？

ということで, [pass](https://www.passwordstore.org/)[^comp1] を使ったパスワード管理を紹介します.

## Pass
pass は GPLv2 ライセンスのついた CLI のパスワードマネージャです.
パスワードは GnuPG で暗号化されたファイルで管理されています.
サブコマンドに git が使え, 適当な場所を remote にして同期したり,
パスワードの変更に対してもバージョン管理の恩恵を受けることができて便利そうです(私は使ってませんが...).

使い方もシンプルで,
- `pass` : 保存されているパスワードの一覧を表示
- `pass generate NAME N` : N 文字のパスワードを生成して NAME と名付けて保存する
    - デフォルトだとパスワードの文字空間が広く, 全ての記号が使われるが,
      使用可能な記号の種類が少ない場合も多く, よく `-n` オプションをつけて記号を使わないようにします
- `pass insert NAME` : 標準入力でパスワードを受け取り, NAME のパスワードを上書きする
- `pass edit NAME` : パスワードが保存されている NAME ファイル を編集する
- `pass rm NAME` : NAME を削除する
- `pass git GIT_SUBCOMMAND` : git のコマンドが叩ける

みたいな感じです.
パスワードを管理するファイルは, 一行目にはパスワードが書かれていますが,
二行目以降には補助的な情報を `pass edit` 等で追記できるようになっていて,
ID や秘密の質問を書いたりするのに使っています.

`pass init GPG_ID` で初期化するんですが,
`gpg` を使っている関係上自分の gpg key を用意しておく必要があります.

## Pass の運用
Pass はシンプルな CLI ツールです.
シンプルなのは良いことだと思いますが, それだけではパスワード管理に不便です.
ここでは私が **Ubuntu**, **Windows**, **Android** でどう相互運用しているか説明します.

### パスワードファイルの共有
pass 自体には git がサブコマンドとして用意されているので適当な remote を通じてパスワードファイルを共有すればいいんですが,
パスワードのファイル名自体は暗号化されていないので,
public な場所を remote にするとどんなパスワードを管理しているのかは筒抜けだったりします.
それが嫌だったので, パスワードのファイル自体を私は GoogleDrive 内に置いています[^comp2].

pass のファイルはデフォルトだと `~/.password-store` 以下で管理することになっています.
私は GoogleDrive 内のディレクトリへシンボリックリンクを張ってますが,
環境変数 `PASSWORD_STORE_DIR` を指定することで任意のディレクトリへ動かすこともできます.

#### GoogleDrive の共有
Windows は言うまでもなく公式アプリを使えばファイル同期はできますが, 他はそうでもありません.
基本的にアプリやブラウザを通じて必要に応じてダウンロードできますが,
相互で同期し続けるような手段を公式は用意してないようです.
そこで相互の同期のために
- ubuntu : [google-drive-ocamlfuse](https://github.com/astrada/google-drive-ocamlfuse)
- android : [DriveSync](https://play.google.com/store/apps/details?id=com.ttxapps.drivesync&hl=ja&gl=US)

を使っています.

`google-drive-ocamlfuse` は ocaml で書かれた GoogleDrive を FUSE ファイルシステムでマウントする CLI ツールです.
ただしセットアップにブラウザを立ち上げて GoogleDrive の認証をするので, 完全に CLI で完結できるわけではないようです[^comp3].

DriveSync はまぁ, 一般の GoogleDrive 自動同期アプリですね.
無料だとアプリ内広告がついたり 1 ディレクトリしか同期できなかったりしますが,
今回はパスワードファイルの入った 1 ディレクトリしか同期する必要がないので事足りています.

### 別の環境での gpg
先述の通り pass は gpg に依存しているので,
他の環境で pass を使いたければ gpg を入れる必要があります. ubuntu は apt 等で入れれば良いですが, 他は
- windows : [gpg4win](https://www.gnupg.org/download/index.html)
- android : [OpenKeyChain](https://www.openkeychain.org/)

を使っています.

gpg4win は公式で提供されているソフトです. Kleopatra というソフトを通じて鍵を管理できます.

OpenKeyChain は Android で pass を使うアプリが依存しています.

各々, 生成した gpg key を import しましょう.

### 別の環境での pass
pass は unix 向け CLI ツールです. 他の環境では何か別の GUI を使ったり使わなかったりします.

- windows : [QtPass](https://github.com/ijhack/qtpass) か, WSL
- android : [Android Password Store](https://passwordstore.app/)

QtPass は Qt で実装された Pass の GUI です. Qt ベースなのでクロスプラットフォームです.
個人的には CLI が便利なので結局 WSL の terminal を通じて pass を叩いていますが[^comp4]...

Android Password Store は唯一?の Android 向け pass 実装です.
デフォルトでクリップボードに時間制限付きコピーできたり使い勝手が良い印象です.
OpenKeyChain 依存しています.

### まとめ
全体像はこちらになります.
<iframe
    class="slide"
    src="./unix-pass-ponchi.pdf"
    width="100%"
></iframe>


## おまけ: Firefox 拡張の passff
pass へ firefox からアクセスできる拡張 [passff](https://addons.mozilla.org/ja/firefox/addon/passff/) がありますが, ちょっと導入に癖があります.
windows でも *nix 系でも対応しているようですが, [ここ](https://github.com/passff/passff-host) にある通り,
plugin をブラウザへ入れる以外に shell(bat) スクリプトを動かす必要があります.
どうも拡張から, 適当な場所に置いた python スクリプトを実行してデータをやりとりするようで,
上記の shell スクリプトはこの python スクリプトを適当な場所へ置くものです.

更に厄介なことに, windows だと cygwin の利用を想定してか, python と pass の両方がコマンドプロンプトの
PATH から見えていることを前提に bat ファイルが書かれているようです.
私の場合 WSL に pass が入っているので,
`c:/Users/USER_NAME/AppData/Roaming/passff/passff.bat` 辺りにあるファイルを修正し,
```
python C:\Users\USER_NAME\AppData\Roaming\passff\passff.py %*
```
を
```
C:\Windows\System32\bash.exe -l -c "/mnt/c/Users/USER_NAME/AppData/Roaming/passff/passff.py %*"
```
に書き換えることで windows でも動かすことができました.
この場合, wsl 側に python が入っていることが前提になります.
環境に応じてうまいことやってください.

## Footnote
[^comp0]: これも自由なソフトウェアだけで管理できているわけではないが...
[^comp1]: ググラビティが低すぎる...
[^comp2]: 将来自前で private な git 鯖を用意するかもしれない
[^comp3]: GUI 環境で認証だけするみたいなことは可能らしい
[^comp4]: pass のディレクトリへは `/mnt/c/PATH/TO/GDrive/DIR` へシンボリックリンクを張っている
