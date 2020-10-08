# Multiplayer Online Secret Hitler

> Secret Hitler is a social deduction game for 5-10 people about finding and stopping the Secret Hitler.

> Players are secretly divided into two teams: the liberals, who have a majority, and the fascists, who are hidden to everyone but each other. If the liberals can learn to trust each other, they have enough votes to control the elections and save the day. But the fascists will say whatever it takes to get elected, advance their agenda, and win the game.

https://www.secrethitler.com

It is all about lying to each other (for example over voice chat) and having fun.

![Screenshot](docs/screenshot.png)

## Used technologies
* [Haskell](https://www.haskell.org), the leading statically typed, purely functional, lazily evaluated programming language
* [Reflex-DOM](https://github.com/obsidiansystems/obelisk), the most advanced industrial-grade Haskell web framework supporting and enforcing pure classic [Functional Reactive Programming (FRP)](https://wiki.haskell.org/Functional_Reactive_Programming)
* [lens](https://hackage.haskell.org/package/lens), [generic-lens](https://hackage.haskell.org/package/generic-lens), [OverloadedLabels](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedLabels)

## Build

1. Follow step 1 and 2 of https://github.com/obsidiansystems/obelisk/#installing-obelisk. There is no need to perform step 3.
2. Execute

```shell
mkdir test-app
ln -s $(nix-build -A exe --no-out-link)/* test-app/
cp -r config test-app
(cd test-app && ./backend)
```

as described on https://github.com/obsidiansystems/obelisk#locally.

## Develop

[Install Obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk). The following options are available.

- Execute `ob run` for a ghcid window. The web server is updated automatically. `-- $>` code comments are supported.
- Execute `ob repl` for a ghci prompt. Load a module using `:l Game`.
- In the directory `backend`, execute `ob shell` and then `cabal run :test` to execute `main` in `test/main.hs`.

## License

* The images and rules are borrowed from http://www.secrethitler.com and licensed under [Creative Commons license BY–NC–SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)
* The Code is licensed under [GNU General Public License v3.0](https://www.gnu.org/licenses/)
