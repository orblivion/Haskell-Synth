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
data Slope = Slope Float

getProgression (Samples s) (SamplingRate sr) = Progression $ ((fromIntegral s) / (fromIntegral sr))
getNumSamples (Progression p) (SamplingRate sr) = Samples $ floor $ p * fromIntegral sr
getSlope (Progression p) (SignalValue sv) = Slope $ (sv / p)
signalValueFromSlope (Progression p) (Slope s) = SignalValue $ p * s

data Cycle a = Cycle a
data Frequency = Frequency SafeValue 
data Amplitude = Amplitude SafeValue 

cycleFunc func (Cycle val_a ) (Cycle val_b )  = Cycle $ func val_a val_b 

fromCycleProgression (Cycle (Progression cp)) (Frequency f) = Progression (cp / f)
toCycleProgression   (Progression tp) (Frequency f) = Cycle $ Progression (tp * f)

fromCycleSignalValue (Cycle (SignalValue sv)) (Amplitude a) = SignalValue (sv * a)

type BasicFunction = Cycle Progression -> Cycle Progression -> Cycle SignalValue

class Addable a where
    (-:) :: a -> a -> a
    (+:) :: a -> a -> a

instance Addable Progression where 
    (-:) (Progression a) (Progression b) = Progression (a - b)
    (+:) (Progression a) (Progression b) = Progression (a + b)

instance Addable SignalValue where 
    (-:) (SignalValue a) (SignalValue b) = SignalValue (a - b)
    (+:) (SignalValue a) (SignalValue b) = SignalValue (a + b)

instance (Addable a) => Addable (Cycle a) where
    (-:) (Cycle a) (Cycle b) = Cycle (a -: b)
    (+:) (Cycle a) (Cycle b) = Cycle (a +: b)

instance Eq Progression where
    (==) (Progression a) (Progression b) = a == b

instance (Eq a) => Eq (Cycle a) where
    (==) (Cycle a) (Cycle b) = a == b

instance Ord Progression where
    compare (Progression a) (Progression b) = compare a b

instance (Ord a) => Ord (Cycle a) where
    compare (Cycle a) (Cycle b) = compare a b

samplesPerCycle = Cycle $ SamplingRate 100000

oscillator :: BasicFunction -> BasicOscillator
oscillator basicFunc fSig aSig Nothing = oscillator basicFunc fSig aSig (Just $ specialize $ flatSignal 0.5)
oscillator basicFunc fSig aSig (Just pSig) = Signal $ oscillator_ fVals aVals pVals (Cycle (Progression 0)) where
    fVals = sanitize fSig
    aVals = sanitize aSig
    pVals = sanitize pSig
    oscillator_ :: [SafeValue] -> [SafeValue] -> [SafeValue] -> Cycle Progression -> [SignalValue]
    oscillator_ fVals aVals pVals t | t >= (Cycle $ Progression 1) = oscillator_ fVals aVals pVals $ t -: (Cycle $ Progression 1)
				    | otherwise = (fromCycleSignalValue basicFunc_ (Amplitude aVal)): oscillatorRest
        where
            fVal:fRest = fVals
            aVal:aRest = aVals
            pVal:pRest = pVals

            basicFunc_ = basicFunc (Cycle (Progression pVal)) t

            oscillatorRest = oscillator_ fRest aRest pRest (t +: cycleProgressionDelta) 

            progressionDelta = getProgression (Samples 1) (SamplingRate samplesPerSecond)
            cycleProgressionDelta = toCycleProgression progressionDelta (Frequency fVal)


osc_square = oscillator basicFunc where
    basicFunc pw t  | t > pw = Cycle $ SignalValue 1
                    | otherwise = Cycle $ SignalValue (-1)

osc_sawtooth = oscillator basicFunc where
    basicFunc _ t   = (cycleFunc signalValueFromSlope t slope) -: (Cycle $ SignalValue 1 )  where
	slope = cycleFunc getSlope (Cycle $ Progression 1) (Cycle $ SignalValue 2)


osc_triangle = oscillator basicFunc where
    basicFunc pw t   | t > pw    = (cycleFunc signalValueFromSlope t upslope) -: (Cycle $ SignalValue 1 )
                     | otherwise = (cycleFunc signalValueFromSlope (t -: pw) downslope) +: (Cycle $ SignalValue 1 )
	where
		upslope = cycleFunc getSlope pw (Cycle $ SignalValue 2)
		downslope = cycleFunc getSlope ((Cycle $ Progression 1) -: pw) (Cycle $ SignalValue (-2) )


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



osc_sine = oscillator basicFunc where
	basicFunc _ (Cycle (Progression t)) = Cycle $ SignalValue $ sin (2 * pi * t)


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

