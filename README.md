Swiss-Draw
==========

スイスドロー形式のトーナメント運営用Shinyアプリ

## Description
次の機能があります:
* トーナメント抽選機能
* 出場者・対戦結果の保存・読み込み機能
* 順位・対戦結果の表示機能


## Demo
![demo](https://github.com/hosscine/Swiss-Draw/wiki/images/swiss.png)


## Requirement
RとRstudioは必須です

次のRパッケージが必要です:
* shiny
* magrittr
* R6
* hosscine/myfs


## Install
Rstudioで以下のコードを入力してパッケージをインストールし, `git clone`します
```r
install.packages("shiny")
install.packages("magrittr")
install.packages("R6")
install.packages("devtools")

devtools::install_github("hosscine/github")
```

## Usage
Rstudioで以下の手順を実行します:
1. how_to_make_tournament_file.Rを開きます
2. 指示に従って`title.ja`, `title.en`, `member`, `deck`を編集します
3. 編集したhow_to_make_tournament_file.Rをすべて実行します (in Rstudio: ctrl+shift+s)
4. 実行すると`title.en`.tournamentファイルが/tournament以下に生成されます
5. ui.Rを開き, RstudioのRun Appボタンを押してアプリを起動します
6. アプリ右側のBrowse...ボタンを押して`title.en`.tournamentファイルを読み込みます
7. 抽選ボタンを押すとトーナメントを始められます (その際, 成績記録ボタンを押すと, 対戦テーブルが`title.en`.tournamentファイルに上書きされます)

### 成績記録の方法
* 左側・右側のプレイヤーの勝ち数をそれぞれresult, vs以下のプルダウンから選択し，成績記録を押します

### 抽選の方法
* 抽選ボタンを押します　その際ROUNDがインクリメントされます
* 再抽選を押すとROUNDがインクリメントされずに抽選が行われます

### トーナメントの再開の方法
* Browse...ボタンを押して.tournamentファイルを読み込みます
* トーナメントが再開されると, .tournamentファイルを保存した時点(成績記録ボタンを押した時点)の全ての情報が復帰します


## Licence

[MIT](https://github.com/tcnksm/tool/blob/master/LICENCE)


## Author

[hosscine](https://github.com/hosscine)
