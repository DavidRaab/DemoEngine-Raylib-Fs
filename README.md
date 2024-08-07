### About the Project: Raylib + F#

This is a port of my MonoGame + F# project to Raylib. I found Raylib
and thought the API and just the procedural way it offers is in
general far better as the typical OO mess. A procedural API is also better
consumable from F# in my opinion.

Like MonoGame, Raylib doesn't try to be a full game engine so it has only
very basics functionality, but still by default they offer some more features
like basic Camera and also the ability for basic draw commands. Also
3D is out-of-the-box supported.

So overall at the moment I am really impressed by Raylib.

Initially the performance compared to MonoGame was a little bit slower,
but not much. I already implemented some more optimizations and now performance
is much better as the MonoGame project.

# As a stress test

I render boxes, at the moment replaced them with some small 16x16 pixel textures.
Every box move at random direction and random rotation. The random directions
and rotations are periodeically randomized so all boxes constantly move
unpredictable. All boxes have a Sprite Sheet and they switch between 3 Sprites
on periodically interval.

Everything runs single-threaded at the moment. I have implemented some
simple object culling so only visible objects are drawn but still updated
when they are not seen. It scales upto 90,000 boxes (maybe more,
didn't tested beyond that point), and still runs fine when all 90,000 boxes
are shown.

I have a parent system, so i make all boxes a parent to another object
and then let this object rotate and all boxes rotate around that
parent object.

# Benchmarks

I am using a Ryzen 5600 + 16 GiB RAM (3200 Mhz) + GTX 1660 Ti.

|                            | All Visible | Culling (~3000)
| :------------------------- | --------:   | -------:
|  3000 boxes without parent | 2050 fps    | 2600 fps
|  6000 boxes without parent | 1100 fps    | 1750 fps
| 10000 boxes without parent |  660 fps    | 1550 fps
| 40000 boxes without parent |  150 fps    |  750 fps
| 90000 boxes without parent |   50 fps    |  320 fps
|                            |             |
|  3000 boxes with parent    | 1950 fps    | 2600 fps
|  6000 boxes with parent    | 1050 fps    | 2300 fps
| 10000 boxes with parent    |  645 fps    | 2000 fps
| 40000 boxes with parent    |  140 fps    |  600 fps
| 90000 boxes with parent    |   50 fps    |  250 fps

Because of Culling it's hard to "measure" anything as framerate greatly
depends on how much is seen on the screen. For example 10,000 boxes
all shown on the screen without parent has ~650 fps, but when
camera is moved away and nothing is shown then fps goes up to >4,000 fps.

So the "Culling" columns is a rough estimate with default Zoom level
and the screen is full of boxes. Usually this means around 3,000
boxes are still rendered.

# Features implemented

* **Entity**: Everything is an entity and you can add/remove components at runtime.
* **Sprite Sheet & Sprite Sheet Animations**: says it all.
* **Transforms**: Every Entity can have a parent and is positioned, scaled and rotated to its parent
* **Input Handling**: Define a data-structure and get back Actions.
* **Timer System**: Running code periodically or after a specific time-frame that depends on the GameTime
* **RenderTexture**: You specify an internal resolution and the game scales with the display resolution.
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
* Particle System (if that is even needed, you just can use default sprite to show. But in a Particle System it could even further optimize some stuff)

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