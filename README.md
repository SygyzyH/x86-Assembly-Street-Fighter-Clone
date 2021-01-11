# x86-Assembly-Street-Fighter-Clone
A project I did a long time ago. This is written for DOS-BOX v0.74-2, for anyone intereseted in running it. The graphics are, of course, not mine.

## Running instructions
In order to run this, you will need some sort of emulator. As stated before, I used DOS-BOX. After mounting your drive and navigating to the folder where you downloaded this project (instructions to mounting drives can be found in the DOS-BOX website), you will need to first compile, link and than run the program, using the turbo assembly executable, and the turbo linker executable. My copies of these files can be found in the "installation" folder.

To run the script, type:
```
tasm /zi str
tlink /v str
str
```

## How to play
There are two players in the game, the left player (player 1) and the right player (player 2). Both players have the exact same movment capabilities, and can be controlled at the same time (via overriding the keyboard interrupt at the interrupt vector).

Player 1 and player 2 can move left, right, duck and jump using the WASD and IJKL, respectively.
The players can punch each other, and if the puch hits the other player, they will play a short animation and wont be able to move for a short time. Players can punch by pressing 'E' and 'U' respectively.

Once a player has ran out of health, the winning player will play a short animation, and after a couple of seconds the game will end and return control to the OS.
The game can then be rerun again.

If two minutes pass without any player defeating the other one, both players will lose.

The game can be closed at any time by pressing the ESC button, which will clean up and return control to the OS.
