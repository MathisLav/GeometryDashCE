# Geometry Dash for Ti-83 Premium CE and Ti-84+CE

## Keys
__Main Menu__
- `MODE/DEL/CLEAR` : quit
- `Left/Right arrows` : choose a level
- `ENTER/2ND` : run the selected level
- `ALPHA` : edit the selected level
- `[+]` : create a level

__Game__
- `2ND` : jump
- `DEL` : return to the main menu
- `CLEAR` : quit the program
- `ENTER` : pause the game

__Edition__
- `Arrows` : move the cursor and scroll
- `ALPHA+Left/Right` : go to the beginning/end of the level
- `2ND/DEL` : insert/delete a tile
- `F1/F5` : change the selected block
- `123456789` : shortcuts to insert some blocks
- `[+][-]` : insert/delete the column under the cursor
- `Alpha+[+]` : insert 10 columns under the cursor
- `[*]([/]+ENTER)` : insert/delete a block at the top
- `G/S` : insert/delete a change of context (G=Gravity/S=Spaceship)
- `PRGM` : run the level
- `ALPHA+PRGM` : run and begin the level at the cursor location
- `MODE/CLEAR` : quit the level/program

## Compiling
### Windows
First, install **[spasm](https://github.com/alberthdev/spasm-ng/releases)**.
Then, move the executable into the root folder of GeometryDashCE and execute `spasm64.exe -E src/GD.z80 bin/AGD.8xp`.

### Linux
First, install **[spasm](https://github.com/alberthdev/spasm-ng/releases)**.
Open a terminal and type :
- `make build` to compile GeometryDashCE and generate [AGD.8xp](bin/AGD.8xp).
- `make send` to send bin/AGD.8xp to the calculator (requires [tilp](http://lpg.ticalc.org/prj_tilp/)).
- `make build_send` as a shortcut of `make build ; make send`.
- `make sendall` to send all necessary files to run GD (without compiling).
- `make archive` to build a portable archive of GeometryDashCE.


## Information
You can find all the levels created by the community [there](https://tiplanet.org/forum/archives_list.php?cat=Jeux+Levels+Geometry+Dash+ce).

The discussion around the project is available at [this link](https://tiplanet.org/forum/viewtopic.php?f=12&t=19211).
Do not hesitate to [contact me](https://tiplanet.org/forum/ucp.php?i=pm&mode=compose&u=52046).

This program is under the [Creative Commons BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).
 

## How can I create my own level on my computer ?
- Download and install Tiled, a software that will allow you to create a level from a specific tileset.
- In Tiled, create a level with a height of at least 10 tiles, then import the [tileset](images/GD.png) (Map -> New Tileset).
- When you're satisfied of your level, export it with the .CSV extension.
- Then, go to http://geometrydash.fr.nf and copy/paste the content of the .CSV file in the "data" field. Give it a name, a speed and a difficulty. 
- If your level has a spaceship or a reversed gravity context, put them in the provided fields : each time you want to insert a new context, just put the tile numbers of the context changes separated by commas (ex : "12,45,48,127" = [12,45[U[48,127[)
- Once you download the Appvar, upload it into you calculator and.. you're done!

> **Warning**
>
> Make sure you :
> - do not end your level with a reversed gravity context
> - are not in a reversed gravity and spaceship context at the same time
> - do not put a changing of context just before the end of the level

> **Note** :
>
> if the previous website is down, please use this one : http://geometrydash.alwaysdata.net <br />
> If neither of the latter works, [please contact me](https://tiplanet.org/forum/ucp.php?i=pm&mode=compose&u=52046).
