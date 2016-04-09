This is a synth in Haskell. It's still rather limited in feature, at this point
it's just a fun project.

To get it working on Ubuntu Wily:

    sudo apt-get install ghc6 c2hs cabal-install libpulse-dev libsndfile1-dev

`c2hs` ought to be gotten via cabal, but it didn't work with the stack snapshot
I used. If somebody knows why I'd be happy to put it in my cabal package setup
instead.

Then the usual for building:

    cabal sandbox init
    cabal install --dependencies-only
    cabal build

As of now (unless I forgot to upgrade this README) the sound it outputs is hard coded. So, just run it, and you'll hear something, or it will generate a file.

Run it with default options. It should pause for a couple seconds, and then you should hear some sort of sound. Hopefully nothing to offensive to your ears, I make no promises though! (turn down your speakers)

    dist/build/HaskellSynth/HaskellSynth

Try it in "realtime" mode. This one buffers the sound into small chunks, instead of the whole sound at once. This is of course a requirement it's set to output a sound that doesn't have a finite length. But, it may be choppy depending on how complicated the sound is, and how fast your computer is:

    dist/build/HaskellSynth/HaskellSynth --realtime

Output the sound to a file:

    dist/build/HaskellSynth/HaskellSynth outputfile.wav

Let me know if it doesn't work and I'll add the step I forgot!

Example Output:

    # TODO - I need to actually host the following files again. Sorry :-(

Here's more or less the first real beat I made. This was the hardcoded output at one point. I'm too lazy to find which revision number it was:

http://static.danielkrol.com/assorted/synth/beat.ogg

Added the closest thing I could to a hi hat. More like a tweet, but it serves about the same purpose:

http://static.danielkrol.com/assorted/synth/beatwithhats.ogg
