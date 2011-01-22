import Sound.Pulse.Simple

samplesPerSecond :: (Num a) => a
samplesPerSecond = 44100

type SafeValue = Float
data SignalValue = SignalValue SafeValue
data Signal = Signal [SignalValue]

flatSignal val = toSignal [val, val..]

takeSeconds s (Signal sigdata) = take (s * samplesPerSecond) sigdata

a :: Float
a = 1.0 / samplesPerSecond

sanitize (SignalValue val) = val
toSignal :: [SafeValue] -> Signal
toSignal values = Signal $ map SignalValue values

osc_sin :: Signal -> Signal -> Signal
osc_sin frequencySig amplitudeSig = toSignal [ampVal * (sin $ 2*pi*freqVal*(t/samplesPerSecond)) | (t, ampVal, freqVal) <- (zip3 [1..] ampVals freqVals)] where
    Signal ampSigVals = amplitudeSig
    Signal freqSigVals = frequencySig
    ampVals = map sanitize ampSigVals
    freqVals = map sanitize freqSigVals

mySin = map sanitize $ takeSeconds 2 $ osc_sin (toSignal [220.0, 220.002 ..]) (toSignal [1.0, 0.999988 ..] )

main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s mySin
    simpleDrain s
    simpleFree s
