import Sound.Pulse.Simple

samplingRate = 44100.0

type SigValue = Float
data Signal = Signal [SigValue]

flatSignal val = Signal [val, val..]

takeSeconds s = take (s * samplingRate)

a :: Float
a = 1.0 / samplingRate

osc_sin :: Signal -> Signal -> Signal
osc_sin frequencySig amplitudeSig = Signal [ampVal * (sin $ 2*pi*freqVal*(t/samplingRate)) | t <-[1..], ampVal <- ampVals, freqVal <- freqVals ] where
    Signal ampVals = amplitudeSig
    Signal freqVals = frequencySig

-- mySin = ([tapering t * (sin $ 2*pi*440*(t/rate))|t<-[1..rate*seconds]] :: [Float]) where
--     tapering t = rate / (rate + t)
--     rate = 44100
--     seconds = 2

mySin = takeSeconds 2 $ osc_sin flatSignal 440 flatSignal 1

main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s mySin
    simpleDrain s
    simpleFree s
