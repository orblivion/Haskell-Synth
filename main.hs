import Components
import Signals 


amp_lfo = sig_adder [(flatSignal 0.75), raw_lfo] where
    raw_lfo = osc_sin (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5 )

freq_lfo = sig_adder [(flatSignal 440), raw_lfo] where
    raw_lfo = osc_sin (specialize $ flatSignal 5 ) (specialize $ flatSignal 50 )

main_sin = osc_sin (specialize freq_lfo ) (specialize amp_lfo  )

mySin = takeSeconds 2 main_sin



main = play $ specialize mySin
