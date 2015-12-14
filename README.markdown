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

**Download ALPHA .jar:** <https://bitbucket.org/sjl/silt/downloads/silt-0.5.0-standalone.jar>

![Preview](http://i.imgur.com/NkEIerF.gif)

## What is it?

You are the god of a toroidal world.  The world is inhabited by creatures who
reproduce (and mutate).

What you choose to do is up to you.  There's no set goal.  If you're feeling
generous, help the creatures prosper and thrive.  Or change the environment to
guide their evolution in a particular direction.  Or maybe you're a vengeful god
and want to destroy all life.

## Interface

![UI Preview](http://i.imgur.com/t87cVC8.png)

## Controls

The theme is "Two Button Controls", but I've bent that a little bit.

* **`arrow keys`** to move your view of the world.
* **`R`** reset the world.
* **`escape`** quit the game.

Put your cursor over a creature to see their stats.

* **`hjkl`** or **`wasd`** to move your cursor.

The world ticks along, but you can freeze time:

* **`space`** pause/unpause time.

Those are the basic controls.  To actually interact with the world you have two
options:

* **`+`** make the world one degree hotter.
* **`-`** make the world one degree colder.


## Game Mechanics

Creatures have energy.  They can spend it to stay alive, and get it by being
near water or eating fruit.

Creatures have a body temperature that needs to stay near their ideal
temperature.  If the world is hotter or colder than they are, they need to spend
energy to maintain their temperature (think sweating or burning calories to warm
up).

Being in water helps creatures regulate their temperature.  Some might evolve to
take advantage of this.

Creatures have an insulation rating that affects how fast or slow they exchange
heat to/from the environment (think different amounts of fur or skin).

Creatures can reproduce asexually if they have enough energy.  Their offspring
may have mutations (different colors, amounts of insulation, tendencies to walk
in a certain direction, etc).

## Mysteries

There are eight mysterious objects scattered across the planet.  What are they
for?

## License

Copyright © 2015 Steve Losh and contributors

Distributed under the MIT/X11 license.
