# secret-hitler

Eine Haskell Implementierung des Spiels Secret Hilter (http://www.secrethitler.com).
Hierbei handelt es sich um ein Gesellschaftsspiel für 5 bis 10 Spieler, bei dem es darum geht, den geheimen Hitler zu entlarven und zu stoppen.
Die Spieler treten in 2 Teams gegeneinander an, die Liberalen und die Faschisten.
Die Liberalen sind in der Überzahl und müssen herausfinden wer die geheimen Übeltäter sind, während Rundenweise Regierungen gute und schlechte Politiken erlassen.
Es geht darum sich gegenseitig anzulügen (zum Beispiel über Voice-Chat) und Spaß zu haben.

## Develop

- Execute `ob run` for a ghcid window. The web server is updated automatically. `-- $>` code comments are supported.
- Execute `ob repl` for a ghci prompt. Load a module using `:l Game`.
- In the directory `backend`, execute `ob shell` and then `cabal run :test` to execute `main` in `test/main.hs`.
