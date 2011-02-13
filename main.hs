import Components
import Signals 

lfo_env = slideEnvelope [(5, 1), (30, 2), (10, 20), (10, 20)]
amp_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5) Nothing
freq_lfo = osc_sine (specialize $ lfo_env ) (specialize $ flatSignal 20 ) Nothing where



-- square_instr sequence = squares sequence 0 where
--     squares sequence t  | t < (len * samplesPerSecond) = (envFunc points t): envelope_ points (t + 1)
--                         | otherwise = square (tail sequence ) 0
--         where
--             (val, len):_ = sequence 
-- 
--     square = osc_square freq amp Nothing
--     freq = specialize $ flatSignal pitch --sig_adder [(flatSignal pitch), freq_lfo]
--    amp = specialize $ sig_adder [(flatSignal 0.75)]--, amp_lfo]
--     amp = specialize $ slideEnvelope [(0, 0.001), (0.2,0.2), (0.1,0.5), (0,100), (0, 100) ]


-- square_chorus = sig_adder $ map square_instr 

-- square_sequence = catSignals $ (map hit [440, 523.25, 659.26]) ++ [square_chorus] where
--    hit val = (takeSeconds 0.3 $ square_instr val)



square_sound = osc_square (specialize freq_lfo ) (specialize amp_lfo  ) Nothing
sine_sound = osc_sine (specialize freq_lfo ) (specialize amp_lfo  ) Nothing

pw_env = slideEnvelope [(0.8, 4), (1, 2), (1,20)]

tri_sound pitch = osc_triangle (specialize $ sig_adder [(flatSignal pitch), freq_lfo ]) (specialize $ flatSignal 0.5 ) $ Just (specialize $ pw_env )

tri_chorus = sig_adder $ map tri_sound [ 300, 301 ]

-- main = play $ specialize $ takeSeconds 4 $ tri_sound
main = writeSound (specialize $ takeSeconds 4 $ tri_chorus) "out.wav"
