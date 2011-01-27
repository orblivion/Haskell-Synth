import Components
import Signals 

amp_lfo = sig_adder [(flatSignal 0.75), raw_lfo] where
    raw_lfo = osc_sine (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5 )

freq_lfo = sig_adder [(flatSignal 440), raw_lfo] where
    raw_lfo = osc_square (specialize slider ) (specialize $ flatSignal 50 ) where
        slider = toSignal [5, 5.0001 ..]

-- main_sig = osc_square (specialize freq_lfo ) (specialize amp_lfo  )
main_sig = osc_sine (specialize freq_lfo ) (specialize $ amp_lfo  )

main = play $ specialize $ takeSeconds 4 main_sig
