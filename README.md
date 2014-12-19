This is [Ian Weller](https://github.com/ianweller)'s final project for EECS 776 at KU.
It's a rudimentary Scrabble-like game, written in Haskell, playable on the console.

Scrabble.hs requires the [random](http://hackage.haskell.org/package/random) library.
This is installed on the EECS Linux machines. It's used to shuffle the letter bag.

Scrabble.hs also requires a dictionary file to exist at `/usr/share/dict/words`.
Valid words are listed in this file and consist of the letters `['a'..'z']`.

This program draws the board using full-width fonts so that the board looks
mostly square. If your system or terminal emulator does not support full-width
fonts, I am concerned that you are using a very old operating system.

When the game starts, you can enter commands like `play function f8 across` or
`play stamp c3 down`. The game will verify the word is allowed, that you can
actually play the word, and then score it for you.

There are some discrepancies between actual Scrabble rules and this game:
* After the first turn, your word placement is not checked and you can place
  a word anywhere.
* Neighboring letters are not checked for real words nor scored.
* Bonus squares are not cleared when a letter has been placed on them.
* The game does not end when someone goes out. The game will never end. The
  Scrabble will go on forever. It is eternal. Scrabble has seen both life and
  death but has never experienced the latter.
* Using all your letters (a "bingo") does not give you an extra 50 points.

These are mostly somewhat easy to implement/fix but if you check the commit
time for when this README was added, you will understand why these problems
were not resolved. I believe the project is sufficiently "difficult" enough
for submission in this state.

If you are a lawyer who thinks this code is infringing your client's trademark
or other rights, please kindly wait to submit a DMCA takedown notice of this
repository until after my instructor has graded this assignment.
