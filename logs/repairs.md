修正点
======

XML PUSHER
----------

### 対応済

### 予定

* 明示的にNothingを渡す代わりに以下の最初に引数を与えることにする
	+ noNeedResponse :: XmlNode -> Bool

HTTP PULL Client
----------------

### Plain

#### 対応済

#### 予定

* pollの間隔を引数として渡せるようにする (Nothingも可能とする)
* pollの間隔を途中で変えられるようにする
* パスを送信ごとに変えられるようにする
	+ [x] XmlPusherクラスへの変更が必要
	+ [x] 送信データに送信データのタイプを指定できるようにするか
	+ [x] その場合、返信の必要性を指定するBool値をそこに含めることが可能かも
	+ [ ] HTTP PULL用のタイプ型を作成する
	+ [ ] 上記のタイプ型を他と共通で使うようにする
		- testPusherの型に(PushedType xp ~ TheType =>)をつける

### TLS

#### 予定

HTTP PULL Server
----------------

### Plain

#### 予定

### TLS

#### 予定

HTTP PUSH
---------

### Plain

#### 予定

### TLS

#### 予定
