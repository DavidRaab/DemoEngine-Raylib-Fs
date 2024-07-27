### About the Project: Raylib + F#

This is a port of my MonoGame + F# project to Raylib. I found Raylib
and thought the API and just the procedural way it offers is in
general far better as the typical OO mess. A procedural API is also better
consumable from F# in my opinion.

Like MonoGame, Raylib doesn't try to be a game engine so it has only
very basics functionality, but still by default they offer some more features
like basic Camera and also the ability for basic draw commands.

So overall at the moment I am really impressed by Raylib.

I ported the whole code and it is nearly feature complete to the MonoGame
project. Only Input Handling must be revemped.

Initially the performance compared to MonoGame was a little bit slower,
but not much. I already implemented some more optimizations and now performance
is much better as the MonoGame project.

# As a stress test

I render boxes, at the moment replaced them with some small 16x16 pixel textures.
Every box move at random direction and random rotation. The random directions
and rotations are periodeically randomized so all boxes constantly move
unpredictable. All boxes have a Sprite Sheet and they switch between 3 Sprites
on period interval.

Everything runs single-threaded at the moment. I have implemented some
simple object culling so only visible objects are drawn but still updated
when they are not seen. It scales upto 90,000 boxes, and still runs fine
when all 90,000 boxes are shown.

I have a parent system, so i make all boxes a parent to another object
and then let this object rotate and all boxes rotate around that
parent object.

# Benchmarks

|                            |   All     | Culling
|----------------------------------------+----------
|  3000 boxes without parent | 2050 fps  | 2700 fps
|  6000 boxes without parent | 1120 fps  | 1950 fps
| 10000 boxes without parent |  690 fps  | 1550 fps
| 40000 boxes without parent |  180 fps  |  900 fps
| 90000 boxes without parent |   90 fps  |  450 fps
|                            |           |
|  3000 boxes with parent    | 1700 fps  | 2500 fps
|  6000 boxes with parent    |  900 fps  | 1300 fps
| 10000 boxes with parent    |  550 fps  |  800 fps
| 40000 boxes with parent    |  130 fps  |  300 fps
| 90000 boxes with parent    |   60 fps  |  190 fps

# Features implemented

* **Entity**: Everything is an entity and you can add/remove components at runtime.
* Sprite Sheet & Sprite Sheet Animations
* **Transforms**: Every Entity can have a parent and is positioned, scaled and rotated to its parent
* **Camera**: It supports multiple cameras that you can move or Zoom
* **Input Handling**: *TODO*
* **Timer System**: Running code periodically or after a specific time-frame that depends on the GameTime
* **RenderTexture**: You specify an internal resolution and the game scales with the display resolution
* **Fixed Update**: A fixed update loop that runs as often as needed per seconds independent from the frame-rate.

# Features to come

## Important

When those are implemented, it is possible to create some games.

* UI
* Scenes / LevelEditor (Loading)
* Multiple GameTime's
* State Machine or Behaviour Trees
* Sound / Music

## Secondly

Neat feature to have, but some games work without those.

* Animation System
* Basic Collision System maybe Physics
* Particle System

## Least important

* Separation into its own library.

# Stability

Currently nothing is stable. The whole API and data-structures will likely change
over time as I see fit to it. The engine will only target F# and no other .NET language.

### License

[![CC0 1.0 Universal](https://licensebuttons.net/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

License: ([CC0 1.0 Universal](http://creativecommons.org/publicdomain/zero/1.0/)) You're free to use the code in any project, personal or commercial. There's no need to ask permission before using these. Giving attribution is not required, but is greatly appreciated!

### Asset Licenses

Some Assets are not my own see Resources.md for those Licenses!