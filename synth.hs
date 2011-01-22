import Sound.Pulse.Simple

samplesPerSecond :: (Num a) => a
samplesPerSecond = 44100

type SigValue = Float
data Signal = Signal [SigValue]

flatSignal val = Signal [val, val..]

takeSeconds s (Signal sigdata) = take (s * samplesPerSecond) sigdata

a :: Float
a = 1.0 / samplesPerSecond

osc_sin :: Signal -> Signal -> Signal
osc_sin frequencySig amplitudeSig = Signal [ampVal * (sin $ 2*pi*freqVal*(t/samplesPerSecond)) | (t, ampVal, freqVal) <- (zip3 [1..] ampVals freqVals)] where
    Signal ampVals = amplitudeSig
    Signal freqVals = frequencySig

-- mySin = ([tapering t * (sin $ 2*pi*440*(t/rate))|t<-[1..rate*seconds]] :: [Float]) where
--     tapering t = rate / (rate + t)
--     rate = 44100
--     seconds = 2

mySin = takeSeconds 2 $ osc_sin (Signal [220.0, 220.002 ..]) (Signal [1.0, 0.999988 ..] )

main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s mySin
    simpleDrain s
    simpleFree s
