import Components
import Signals 

amp_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5) Nothing
freq_lfo = osc_sine (specialize $ flatSignal 10 ) (specialize $ flatSignal 5 ) Nothing where





square_instr pitch = osc_square freq amp Nothing where
    freq = specialize $ flatSignal pitch --sig_adder [(flatSignal pitch), freq_lfo]
--    amp = specialize $ sig_adder [(flatSignal 0.75)]--, amp_lfo]
    amp = specialize $ envelope [(0, 0.001), (0.4,0.2), (0.2,0.5), (0,100), (0, 100) ]


square_chorus = sig_adder $ map square_instr [440, 523.25, 659.26 ]

square_sequence = catSignals $ (map hit [440, 523.25, 659.26]) ++ [square_chorus] where
    hit val = (takeSeconds 0.3 $ square_instr val)



square_sound = osc_square (specialize freq_lfo ) (specialize amp_lfo  ) Nothing
sine_sound = osc_sine (specialize freq_lfo ) (specialize amp_lfo  ) Nothing










main = play $ specialize $ takeSeconds 4 $ square_sequence
