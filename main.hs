import Components
import Signals 

lfo_env = envelope [(5, 4), (30, 10), (30, 20)]
amp_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5) Nothing
freq_lfo = osc_sine (specialize $ lfo_env ) (specialize $ flatSignal 400 ) Nothing where



square_instr pitch = osc_square freq amp Nothing where
    freq = specialize $ flatSignal pitch --sig_adder [(flatSignal pitch), freq_lfo]
--    amp = specialize $ sig_adder [(flatSignal 0.75)]--, amp_lfo]
    amp = specialize $ envelope [(0, 0.001), (0.4,0.2), (0.2,0.5), (0,100), (0, 100) ]


square_chorus = sig_adder $ map square_instr [440, 523.25, 659.26 ]

square_sequence = catSignals $ (map hit [440, 523.25, 659.26]) ++ [square_chorus] where
    hit val = (takeSeconds 0.3 $ square_instr val)



square_sound = osc_square (specialize freq_lfo ) (specialize amp_lfo  ) Nothing
sine_sound = osc_sine (specialize freq_lfo ) (specialize amp_lfo  ) Nothing

pw_env = envelope [(0.5, 4), (1, 2), (1,20)]
tri_sound = osc_triangle (specialize $ flatSignal 440) (specialize $ flatSignal 1 ) $ Just (specialize $ pw_env )


main = play $ specialize $ takeSeconds 4 $ tri_sound
