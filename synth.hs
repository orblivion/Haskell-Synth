import Sound.Pulse.Simple

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
data AmplitudeSignal = AmplitudeSignal [SignalValue]

instance SpecializedSignal FrequencySignal where
    specialize (Signal sigvalues) = FrequencySignal sigvalues
    sanitize (FrequencySignal sigvalues) = map sanitize sigvalues where
        sanitize (SignalValue sigvalue)     | sigvalue < 0 = 0
                                            | sigvalue > 20000 = 20000
                                            | otherwise = sigvalue


instance SpecializedSignal AmplitudeSignal where
    specialize (Signal sigvalues) = AmplitudeSignal sigvalues
    sanitize (AmplitudeSignal sigvalues) = map sanitize sigvalues where
        sanitize (SignalValue sigvalue)   | sigvalue < 0 = 0
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

mySin = fromSignal $ takeSeconds 2 $ osc_sin (specialize $ toSignal [220.0, 220.002 ..]) (specialize $ toSignal [1.0, 0.999988 ..] )

main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s mySin
    simpleDrain s
    simpleFree s
