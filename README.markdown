    :'######::'####:'##:::::::'########:
    '##... ##:. ##:: ##:::::::... ##..::
     ##:::..::: ##:: ##:::::::::: ##::::
    . ######::: ##:: ##:::::::::: ##::::
    :..... ##:: ##:: ##:::::::::: ##::::
    '##::: ##:: ##:: ##:::::::::: ##::::
    . ######::'####: ########:::: ##::::
    :......:::....::........:::::..:::::

Silt is my little game for [Ludum Dare
34](http://ludumdare.com/compo/2015/12/09/welcome-to-ludum-dare-34/).  The theme
this time was a tie between "Growing" and "Two Button Controls".

![Preview](https://i.imgur.com/oTXGd2z.gif)

## What is it?

You are the god of a toroidal world.  The world is inhabited by creatures who
reproduce (and mutate).

What you choose to do is up to you.  There's no set goal.  If you're feeling
generous, help the creatures prosper and thrive.  Or change the environment to
guide their evolution in a particular direction.  Or maybe you're a vengeful god
and want to destroy all life.

## Controls

*Not all of this is implemented yet.*

The theme is "Two Button Controls", but I've bent that a little bit.

* **`hjkl`** to move your view of the world.
* **`arrow keys`** to move the view a bit faster.
* **`r`** reset the world.
* **`escape`** quit the game.

The world ticks along at 2 ticks per second, but you can freeze time:

* **`space`** pause/unpause time.

You can tick the world yourself too:

* **`123456789`** to "tick" the world.  **`1`** runs for one tick, **`2`** for
  ten ticks, **`3`** for one hundred ticks, etc.

Those are the basic controls.  To actually interact with the world you have two
options:

* **`+`** make the world one degree hotter.
* **`-`** make the world one degree colder.

## Game Mechanics

*Not all of this is implemented yet.*

Creatures have energy.  They can spend it to stay alive, and get it by being
near food/water.

Creatures have a body temperature that needs to stay near their ideal
temperature.  If the world is hotter or colder than they are, they need to spend
energy to maintain their temperature (think sweating or burning calories to warm
up).

Creatures have an insulation rating that affects how fast or slow they exchange
heat to/from the environment (think different amounts of fur or skin).

Creatures will eventually die of old age.

Creatures can reproduce asexually if they have enough energy.  Their offspring
may have mutations (different colors, ideal temperatures, amounts of insulation,
etc).

## Mysteries

There are a couple of strange object scattered around the landscape.  Can you
find out what they do?

## License

Copyright © 2015 Steve Losh and contributors

Distributed under the MIT/X11 license.
