# Geometry Dash for TI-83 Premium CE and TI-84+CE

https://user-images.githubusercontent.com/85101130/137631753-f7c3c936-968e-44e1-982f-c042ceb7c46b.mp4



### Keys
__Main Menu__
- MODE/DEL/CLEAR : quit
- </> : choose a level
- ENTER/2ND : run the selected level
- ALPHA : edit the selected level
- \+ : create a level!


__Game__
- 2ND : jump
- DEL : return to the main menu
- CLEAR : quit the program
- ENTER : pause the game

__Edition__
- Arrows : move the cursor and scroll
- ALPHA+Left/Right : go to the beginning/end of the level
- 2ND/DEL : insert/delete a tile
- F1/F5 : change the selected block
- 123456789 : shortcuts to insert some blocks
- [+][-] : insert/delete the column under the cursor
- Alpha+[+] : insert 10 columns under the cursor
- [*][/ et ENTRER] : insert/delete a block at the top
- G/S : insert/delete a change of context (G=Gravity/S=Spaceship)
- PRGM : run the level
- ALPHA+PRGM : run and begin the level at the cursor location
- MODE/CLEAR : quit the level/program

/!\ WARNING! Make sure you :
- do not end your level with a reversed gravity context
- are not in a reversed gravity and spaceship context at the same time
- do not put a changing of context just before the end of the level


### Compiling
#### Windows
In the root folder, execute `spasm64.exe -E src/GD.z80 AGD.8xp`.

#### Linux
This solution requires wine64. On Ubuntu, this can be installed with `sudo apt-get install wine64`.  
Then, in the root folder, execute `make spasm`.


### Information
You can find the other levels here : https://tiplanet.org/forum/archives_list.php?cat=Jeux+Levels+Geometry+Dash+ce

The topic of the project :
https://tiplanet.org/forum/viewtopic.php?f=12&t=19211
Do not hesitate to contact me at this address :
https://tiplanet.org/forum/ucp.php?i=pm&mode=compose&u=52046

This program is under the Creative Commons BY-NC-SA 4.0
More information here : https://creativecommons.org/licenses/by-nc-sa/4.0/
 

### How can I create my own level on computer ?
- Download and install Tiled, a software that will allow you to create a level from a tileset.
- In Tiled, create a level with a height of at least 10 tiles, then import the tileset GD.png in the archive (Map -> New Tileset)
- When you're satisfied, export your level with the .CSV extension
- Then, go to http://geometrydash.fr.nf and copy/paste the content of the .CSV file in the "data" field. Give it a name, a speed and a difficulty. 
- If your level has a spaceship or a reversed gravity context, put them in the correct field : at each time you want to change the context, you just have to put the tile numbers of the changes separated by commas (ex : "12,45,48,127" = [12,45[U[48,127[)
- Once you download the Appvar, upload it into you calculator and.. you're done!


Thanks to Shadow, the archive now contains 8 levels (instead of the initial 3 levels)! Thank you!


NOTE : if the previous website is down, please use this one : http://geometrydash.alwaysdata.net
If neither of the latter works, please contact me : https://tiplanet.org/forum/ucp.php?i=pm&mode=compose&u=52046
