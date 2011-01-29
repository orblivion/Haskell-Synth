module Components where

import Sound.Pulse.Simple
import Signals
import List

type BasicOscillator = FrequencySignal -> AmplitudeSignal -> (Maybe PWMSignal) -> Signal

-- TimeSamplingRate = TimeSamples/Time
-- Frequency = NumCycles/Time
-- Time * Frequency = NumCycles
-- Time * SamplingRate = Samples
-- CycleSamplingRate = CycleSamples/NumCycles

data SamplingRate = SamplingRate Integer
data Samples = Samples Integer
data Progression = Progression Float
data Output = Output SafeValue

getProgression (Samples s) (SamplingRate sr) = Progression $ ((fromIntegral s) / (fromIntegral sr))
getNumSamples (Progression p) (SamplingRate sr) = Samples $ floor $ p * fromIntegral sr

data Time a = Time a
data Cycle a = Cycle a
data Frequency = Frequency SafeValue 
data Amplitude = Amplitude SafeValue 

timeFunc func  (Time val_a )   (Time val_b )    = Time $ func val_a val_b 
cycleFunc func (Cycle val_a ) (Cycle val_b )  = Cycle $ func val_a val_b 

toTime (Cycle (Progression cp)) (Frequency f) = toRational $ cp / f
toCycle (Time (Progression tp)) (Frequency f) = tp * f

fromOutput (Output o) (Amplitude a) = SignalValue $ o * a

type BasicFunction = Cycle Progression -> Cycle Output


-- oscillator :: BasicFunction -> BasicOscillator
-- oscillator basicFunc fSig aSig pSig = oscillator_ basicFunc fSig aSig pSig 0 where
--     fVals = sanitize fSig
--     aVals = sanitize aSig
--     pVals = sanitize pSig
--     oscillator_ basicFunc fSign aSig pSig t | t >= samplesPerSecond = oscillator_ basicFunc fSign aSig pSig (sSub t samplesPerSecond)
--                                             | otherwise = ( aVal * (basicFunc pVal t) ) : oscillator_ basicFunc fRest aRest pRest (sAdd t fVal) 
--         where
--             fVal:fRest = fVals
--             aVal:aRest = aVals
--             pVal:pRest = pVals
        




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
osc_square frequencySig amplitudeSig _ = toSignal $ [ squarefunc t freqVal ampVal | (t, ampVal, freqVal) <- zip3 [0..] ampVals freqVals ] where
    ampVals = sanitize amplitudeSig
    freqVals = sanitize frequencySig
    squarefunc :: SafeValue -> SafeValue -> SafeValue -> SafeValue
    squarefunc t freqVal ampVal | (mod (floor $ t*freqVal) samplesPerSecond) < floor (toRational $ samplesPerSecond / 2) = ampVal
                                | otherwise = 0 - ampVal



osc_sine :: BasicOscillator 
osc_sine frequencySig amplitudeSig _ = toSignal [(sin $ 2*pi*freqVal*(t/samplesPerSecond)) | (t, ampVal, freqVal) <- (zip3 [1..] ampVals freqVals)] where
    ampVals = sanitize amplitudeSig
    freqVals = sanitize frequencySig



sig_adder :: [Signal] -> Signal
sig_adder insignals = toSignal outvalues where
    invalues = map fromSignal insignals
    outvalues = map sum $ transpose invalues



envelope :: [(SafeValue, Float)] -> Signal
envelope points = toSignal $ envelope_ points 0 where
    envelope_ points t  | t < (len * samplesPerSecond)   = (val + (t * slope)): envelope_ points (t + 1)
                        | otherwise = envelope_ (tail points ) 0
        where
            (val, len):(next_val, _):_ = points
            slope = (next_val - val) / (len * samplesPerSecond)



play :: SoundSignal -> IO ()
play signal = do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s $ sanitize signal 
    simpleDrain s
    simpleFree s

