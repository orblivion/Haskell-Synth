import Components
import Signals 
import System.Environment( getArgs )

lfo_env = slideEnvelope [(5, 1), (30, 2), (10, 20), (10, 20)]
amp_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5) Nothing
freq_lfo = osc_sine (specialize $ lfo_env ) (specialize $ flatSignal 20 ) Nothing where

square_sound = osc_square (specialize freq_lfo ) (specialize amp_lfo  ) Nothing
sine_sound = osc_sine (specialize freq_lfo ) (specialize amp_lfo  ) Nothing

------

pw_env = slideEnvelope [(0.8, 4), (1, 2), (1,20)]

tri_sound pitch = osc_triangle (specialize $ sig_adder [(flatSignal pitch), freq_lfo ]) (specialize $ flatSignal 0.5 ) $ Just (specialize $ pw_env )

tri_chorus = sig_sequence [([tri_sound 150], Progression 0), ([tri_sound 320], Progression 2)]

------

square_instr sequence = sig_sequence $ map toSigSequence sequence where
    toSigSequence (pitches, progression) = (map square pitches, progression)
    square pitch = osc_square (freq pitch) amp Nothing
    freq pitch = specialize $ flatSignal pitch
    amp = specialize $ slideEnvelope [(0, 0.001), (0.2,0.2), (0.1,0.5), (0,100), (0, 100) ]

square_sequence = square_instr [
    ( [440] , Progression 0),  ( [523.25] , Progression 0.60), ( [659.26] , Progression 0.60) ,
    ( [440] , Progression 0.60),  ( [523.25] , Progression 0.10), ( [659.26] , Progression 0.15)
    ]

-------


kick_instr sequence = sig_sequence $ map toSigSequence sequence where
    toSigSequence progression = ([kick], progression)
    kick = osc_triangle (specialize f_env) (specialize a_env) Nothing where
    f_env = slideEnvelope [(100, 0.1), (20, 100), (20, 100)]
    a_env = slideEnvelope [(0, 0.001), (0.4,0.1), (0,0.5), (0,100), (0, 100)]


chime_instr sequence = sig_sequence $ map toSigSequence sequence where
    toSigSequence progression = ([chime], progression)
    chime = osc_triangle (specialize $ flatSignal 5000) (specialize $ sig_adder [a_env, a_lfo]) (Just (specialize $ flatSignal 0.95))
    a_env = slideEnvelope [(0, 0.001), (0.1,0.5), (0.05,2), (0,100), (0, 100)]
    a_lfo = osc_sine (specialize $ flatSignal 3) (specialize a_env) Nothing where
        a_env = slideEnvelope [(0.05, 2.5), (0, 100), (0, 100)]

bass_instr sequence = catSignals $ map bass sequence where
    bass (pitch, Progression p) = takeSeconds p $ osc_triangle (specialize $ sig_adder [freq pitch]) (amp p) (Just $ specialize $ flatSignal 0.8)
    freq pitch = flatSignal pitch
    freq_lfo = osc_triangle (specialize $ flatSignal 15) (specialize $ flatSignal 5) Nothing
    amp p = specialize $ slideEnvelope [ (0, 0.001), (0.4, p - 0.2), (0.4,0.2), (0,100), (0,100) ]


kick_sequence = kick_instr $ (Progression 0):( cycle [ Progression 0.2, Progression 0.4, Progression 0.2, Progression 0.4, Progression 0.2, Progression 0.1, Progression 0.1])
chime_sequence = chime_instr $ [Progression 1 ] ++ repeat ( Progression 1.6 )
bass_sequence = bass_instr $ cycle [ (41.2, Progression 0.2), (41.2, Progression 0.6),  (43.65, Progression 0.4),  (38.8, Progression 0.4) ]

the_sound = specialize $ takeSeconds 6 $ sig_adder $ [bass_sequence, chime_sequence, kick_sequence]

-- main = 
-- main = playRealtime the_sound
-- play the_sound

handleSound [] = play the_sound
handleSound ["--realtime"] = playRealtime the_sound
handleSound [filename] = writeSound the_sound filename 
handleSound _ = return ()

main = do
    args <- getArgs
    handleSound args
