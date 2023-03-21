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

# Example Output:

I made various sounds over the course of making this. I lost track of what sound went with what version of this codebase. I don't think any of these correspond to the latest version of the code. I'm just going to post these here for posterity.

Warning, these might be a little loud.

* [A beat sequence](example-output/beat.ogg?raw=true) - Possibly the first one I made.
* [Similar beat with something like high hats](example-output/beatwithhats.ogg?raw=true) - More like tweets
* [LFO modulated by something, probably an envelope](example-output/example.ogg?raw=true) and it sounds like also modulating the square pulse width (no filters were used)
* [LFO modulated by something, probably an envelope, with a second sound](example-output/example2.ogg?raw=true)
* [A minor chord](example-output/square_sequence_2.ogg?raw=true)
* [A minor chord 2](example-output/square_sequence.ogg?raw=true)
