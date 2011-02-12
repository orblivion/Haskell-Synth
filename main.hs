import Components
import Signals 

lfo_env = slideEnvelope [(5, 1), (30, 2), (10, 20), (10, 20)]
amp_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5) Nothing
freq_lfo = osc_sine (specialize $ lfo_env ) (specialize $ flatSignal 20 ) Nothing where



square_instr pitch = osc_square freq amp Nothing where
    freq = specialize $ flatSignal pitch --sig_adder [(flatSignal pitch), freq_lfo]
--    amp = specialize $ sig_adder [(flatSignal 0.75)]--, amp_lfo]
    amp = specialize $ slideEnvelope [(0, 0.001), (0.4,0.2), (0.2,0.5), (0,100), (0, 100) ]


square_chorus = sig_adder $ map square_instr [440, 523.25, 659.26 ]

square_sequence = catSignals $ (map hit [440, 523.25, 659.26]) ++ [square_chorus] where
    hit val = (takeSeconds 0.3 $ square_instr val)



square_sound = osc_square (specialize freq_lfo ) (specialize amp_lfo  ) Nothing
sine_sound = osc_sine (specialize freq_lfo ) (specialize amp_lfo  ) Nothing

pw_env = slideEnvelope [(0.8, 4), (1, 2), (1,20)]
measure_length = 0.075 * 16
break_length = 0.015
sixteenth_on = [(1, fromRational (measure_length / 16) - break_length ), (0, break_length)]
sixteenth_off = [(0,fromRational (measure_length / 16))]
eighth_on = [(1, fromRational (measure_length / 8) - break_length ), (0, break_length)]
eighth_off = [(0,fromRational (measure_length / 8))]
quarter_on = [(1, fromRational (measure_length / 4) - break_length ), (0, break_length)]
quarter_off = [(0,fromRational (measure_length / 4))]

amp_env = stepEnvelope $ cycle $ concat [
    eighth_on, sixteenth_on, sixteenth_on, eighth_on, eighth_on, eighth_on, eighth_off, quarter_on,
    eighth_on, quarter_on, eighth_on, quarter_off, sixteenth_on, sixteenth_on, eighth_off 
    ]

tri_sound = osc_triangle (specialize $ sig_adder [(flatSignal 110), freq_lfo ]) (specialize $ flatSignal 0.5 ) $ Just (specialize $ pw_env )

-- main = play $ specialize $ takeSeconds 4 $ tri_sound
main = writeSound (specialize $ takeSeconds 4 $ tri_sound) "out.wav"
