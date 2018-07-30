# ビジネス活用事例で学ぶ データサイエンス入門

R言語で書かれたソースコードをPythonに移植していく。

## 開発環境

```
$ pipenv install
```

## 実行方法

```bash
$ cd data
$ python ../src/ch03_0822.py
```

## 移植リスト

- [x] src/ch03\_0822.R -> src/ch03\_0822.py
- [ ] src/ch04\_0512.R
- [ ] src/ch05\_0512.R
- [ ] src/ch06\_0512.R
- [ ] src/ch07\_0512.R
- [x] src/ch08\_0512.R
- [ ] src/ch09\_0512.R
- [ ] src/ch10\_0512.R

## 注意点

MacOSXだとMatplotlibのレンダリングがCocoaのAPIを使って行われるため下記エラーが出る。

```
RuntimeError: Python is not installed as a framework. The
Mac OS X backend will not be able to function correctly if Python
is not installed as a framework. See the Python documentation for
more information on installing Python as a framework on Mac OS X.
Please either reinstall Python as a framework, or try one of the other
backends.
```

そのため、`~/.matplotlib/matplotlibrc`に`backend: TkAgg`を追加する必要がある。

参考URL: <https://github.com/pypa/pipenv/issues/754>
