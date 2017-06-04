Tulen Engine
============

The purpose of the project is provide scriptable game engine for easy creation
of RTS-like games. The idea is originally inspired by Warcraft III mod making phenomenon,
where an user can create custom maps that can significantly change primordial gameplay.  

So the engine is designed with the following features in mind:

- Open source and vendor lock free. Your maps are your property and the core is shared.

- Maximum scripting freedom. Provide as much configurability as necessary to freely
implement your ideas.

- Easy start for novice. GUI editor for maps that accumulates landscape editing,
resource management and graphical scripting.

- Expression power for an experienced user. If graphical scripting is not enough,
an user can switch to powerful underlying language (Haskell) and define so much
abstraction that he need.

- Fork and go. Feature to embed all your script into engine, change the core and
go with developing completely independent game if you choose to.

- Portability. Target platforms (at the moment) are Windows, GNU/Linux, Mac. Other
platforms are possible in future.

- Built-in networking with synchronization. Minimum hassle with implementing
multiplayer games.

How to build
============

## Compilation of Urho3D

Urho3D is rendering engine that is used for internal implementation. Urho3D should be configured with following options:

TODO: add other platform instructions.

``` bash
git clone https://github.com/urho3d/Urho3D.git
cd Urho3D
git checkout ce69ad556e070d965ad9bda74ae2c441cc59f7be
mkdir build
cd build

cmake .. -DURHO3D_SAMPLES=1 -DURHO3D_EXTRAS=1 -DURHO3D_LIB_TYPE=SHARED -DCMAKE_INSTALL_PREFIX:PATH=/usr

make
sudo make install
```

Also you need to adjust paths at the end of `stack.yml`:

```
extra-lib-dirs:
- /usr/lib64/Urho3D
extra-include-dirs:
- /usr/include
- /usr/include/Urho3D/ThirdParty
```

## Compilation of the engine

You need [stack](https://haskell-lang.org/get-started):

1. `stack install`
