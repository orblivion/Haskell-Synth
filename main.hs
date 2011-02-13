import Components
import Signals 

lfo_env = slideEnvelope [(5, 1), (30, 2), (10, 20), (10, 20)]
amp_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5) Nothing
freq_lfo = osc_sine (specialize $ lfo_env ) (specialize $ flatSignal 20 ) Nothing where

square_instr sequence = sig_sequence $ map toSigSequence sequence where
    toSigSequence (pitches, progression) = (map square pitches, progression)
    square pitch = osc_square (freq pitch) amp Nothing
    freq pitch = specialize $ flatSignal pitch --sig_adder [(flatSignal pitch), freq_lfo]
    amp = specialize $ slideEnvelope [(0, 0.001), (0.2,0.2), (0.1,0.5), (0,100), (0, 100) ]

square_sound = osc_square (specialize freq_lfo ) (specialize amp_lfo  ) Nothing
sine_sound = osc_sine (specialize freq_lfo ) (specialize amp_lfo  ) Nothing

pw_env = slideEnvelope [(0.8, 4), (1, 2), (1,20)]

tri_sound pitch = osc_triangle (specialize $ sig_adder [(flatSignal pitch), freq_lfo ]) (specialize $ flatSignal 0.5 ) $ Just (specialize $ pw_env )

tri_chorus = sig_sequence [([tri_sound 150], Progression 0), ([tri_sound 320], Progression 2)]


square_sequence = square_instr [
    ( [440] , Progression 0),  ( [523.25] , Progression 0.60), ( [659.26] , Progression 0.60) ,
    ( [440] , Progression 0.60),  ( [523.25] , Progression 0.10), ( [659.26] , Progression 0.15)
    ]

the_sound = specialize $ takeSeconds 6 $ square_sequence

main = writeSound the_sound "out.wav"
-- main = play the_sound
