import Sound.Pulse.Simple
import List

samplesPerSecond :: (Num a) => a
samplesPerSecond = 44100

type SafeValue = Float
data SignalValue = SignalValue SafeValue
data Signal = Signal [SignalValue]

flatSignal val = toSignal [val, val..]

takeSeconds s (Signal sigdata) = Signal $ take (s * samplesPerSecond) sigdata

a :: Float
a = 1.0 / samplesPerSecond

class SpecializedSignal s where
    specialize :: Signal -> s
    sanitize :: s -> [SafeValue]

data FrequencySignal = FrequencySignal [SignalValue]

instance SpecializedSignal FrequencySignal where
    specialize (Signal sigvalues) = FrequencySignal sigvalues
    sanitize (FrequencySignal sigvalues) = map sanitize sigvalues where
        sanitize (SignalValue sigvalue)     | sigvalue < 0 = 0
                                            | sigvalue > 20000 = 20000
                                            | otherwise = sigvalue

data AmplitudeSignal = AmplitudeSignal [SignalValue]

instance SpecializedSignal AmplitudeSignal where
    specialize (Signal sigvalues) = AmplitudeSignal sigvalues
    sanitize (AmplitudeSignal sigvalues) = map sanitize sigvalues where
        sanitize (SignalValue sigvalue)   | sigvalue < 0 = 0
                            | sigvalue > 1 = 1
                            | otherwise = sigvalue

data SoundSignal = SoundSignal [SignalValue]

instance SpecializedSignal SoundSignal where
    specialize (Signal sigvalues) = SoundSignal sigvalues
    sanitize (SoundSignal sigvalues) = map sanitize sigvalues where
        sanitize (SignalValue sigvalue)   | sigvalue < -1 = -1
                            | sigvalue > 1 = 1
                            | otherwise = sigvalue

toSignal :: [SafeValue] -> Signal
toSignal values = Signal $ map SignalValue values

fromSignal :: Signal -> [SafeValue]
fromSignal (Signal sigvals) = map fromSigVal sigvals where
    fromSigVal (SignalValue value) = value


osc_sin :: FrequencySignal -> AmplitudeSignal -> Signal
osc_sin frequencySig amplitudeSig = toSignal [ampVal * (sin $ 2*pi*freqVal*(t/samplesPerSecond)) | (t, ampVal, freqVal) <- (zip3 [1..] ampVals freqVals)] where
    ampVals = sanitize amplitudeSig
    freqVals = sanitize frequencySig

sig_adder :: [Signal] -> Signal
sig_adder insignals = toSignal outvalues where
    invalues = map fromSignal insignals
    outvalues = map sum $ transpose invalues

amp_lfo = sig_adder [(flatSignal 0.75), raw_lfo] where
    raw_lfo = osc_sin (specialize $ flatSignal 2 ) (specialize $ flatSignal 0.5 )

freq_lfo = sig_adder [(flatSignal 440), raw_lfo] where
    raw_lfo = osc_sin (specialize $ flatSignal 5 ) (specialize $ flatSignal 50 )

main_sin = osc_sin (specialize freq_lfo ) (specialize amp_lfo  )

mySin = takeSeconds 2 main_sin

play :: SoundSignal -> IO ()
play signal = do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s $ sanitize signal 
    simpleDrain s
    simpleFree s

main = play $ specialize mySin
