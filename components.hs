module Components where

import Sound.Pulse.Simple
import Signals
import List

type BasicOscillator = FrequencySignal -> AmplitudeSignal -> Signal

-- make types for all the different parts of the time equation so I don't get them messed up
-- for instance, t will be of a type that has the domain -1 - 1. stuff like that
-- look up Haskell Fractional too in case that helps

-- osc_sawtooth :: BasicOscillator 
-- osc_sawtooth frequencySig amplitudeSig = toSignal [ampVal * (sawfunc $ freqVal*(t/samplesPerSecond)) | (t, ampVal, freqVal) <- (zip3 timeloop ampVals freqVals)] where
--     sawfunc time = time
--     timeloop = [0 .. samplesPerSecond - 1] ++ timeloop
--     ampVals = sanitize amplitudeSig
--     freqVals = sanitize frequencySig

-- osc_sawtooth :: BasicOscillator 
-- osc_sawtooth frequencySig amplitudeSig = toSignal $ [ sawfunc t freqVal ampVal | (t, freqVal, ampVal) <- zip3 [0..] ampVals freqVals ] where
--     sawfunc :: Integer -> SafeValue -> SafeValue -> SafeValue
--     -- sawfunc t freqVal ampVal = -1 + ampVal * 2 * (fromIntegral $ mod (t*440) samplesPerSecond) / samplesPerSecond
--     sawfunc t freqVal ampVal = -1 + 2 * (fromIntegral $ mod (t*freqVal) samplesPerSecond) / samplesPerSecond
--     ampVals = sanitize amplitudeSig
--     freqVals = sanitize frequencySig

-- t = [-1 + (2*t)/samplesPerSecond] + val
    


-- osc_triangle :: BasicOscillator 
-- osc_triangle frequencySig amplitudeSig = Signal osc_triangle_part frequencySig amplitudeSig 0 where
--     osc_triangle_part frequencySig amplitudeSig t | t < halfway = ampVal * ( 


osc_square :: BasicOscillator 
osc_square frequencySig amplitudeSig = toSignal $ [ squarefunc t freqVal ampVal | (t, ampVal, freqVal) <- zip3 [0..] ampVals freqVals ] where
    ampVals = sanitize amplitudeSig
    freqVals = sanitize frequencySig
    squarefunc :: SafeValue -> SafeValue -> SafeValue -> SafeValue
    squarefunc t freqVal ampVal | (mod (floor $ t*freqVal) samplesPerSecond) < floor (toRational $ samplesPerSecond / 2) = ampVal
                                | otherwise = 0 - ampVal




osc_sine :: BasicOscillator 
osc_sine frequencySig amplitudeSig = toSignal [ampVal * (sin $ 2*pi*freqVal*(t/samplesPerSecond)) | (t, ampVal, freqVal) <- (zip3 [1..] ampVals freqVals)] where
    ampVals = sanitize amplitudeSig
    freqVals = sanitize frequencySig




sig_adder :: [Signal] -> Signal
sig_adder insignals = toSignal outvalues where
    invalues = map fromSignal insignals
    outvalues = map sum $ transpose invalues




play :: SoundSignal -> IO ()
play signal = do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s $ sanitize signal 
    simpleDrain s
    simpleFree s

