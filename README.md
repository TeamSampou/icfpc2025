[![Haskell](https://github.com/TeamSampou/icfpc2025/actions/workflows/haskell.yml/badge.svg)](https://github.com/TeamSampou/icfpc2025/actions/workflows/haskell.yml)

# ICFP Programming Contest 205

This is Team Sampou's repository for the [ICFP Programming Contest 2025](http://icfpcontest2025.github.io/).

## Members

* [Katsutoshi Itoh](https://github.com/cutsea110)
* [Kei Hibino](https://github.com/khibino)
* [Masahiro Sakai](https://github.com/msakai)
* [Nobuo Yamashita](https://github.com/nobsun)
* [Yasuyuki Ogawa](https://github.com/oganet)

## Languages

* Haskell

## Usage

Prepare `.env` file with the following content:

```
ID="..."
```

Run `stack repl`.

You can solve a problem interactively.

```
ghci> initClient
initClient
ghci> select "probatio"
select "probatio"
"probatio"
ghci> explore ["000","123","213","333"]
explore ["000","123","213","333"]
([[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]],5)
ghci> explore ["4","5","02","03","04","05","31","32","34","35"]
explore ["4","5","02","03","04","05","31","32","34","35"]
([[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]],16)0
ghci> guess ([0,1,2], 0, [((0,0),(1,2)), ((0,1),(0,1)), ((0,2),(1,5)), ((0,3),(2,0)), ((0,4),(2,2)), ((0,5),((2,3))), ((1,0),(2,1)), ((1,1),(1,4)), ((1,3), (2,5)), ((2,4),(2,4)), ((2,5),(1,3))])
True
```
