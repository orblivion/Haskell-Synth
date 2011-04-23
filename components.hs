module Components where

import Sound.Pulse.Simple
import Signals
import List

import qualified Data.Vector.Generic as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import qualified Control.Concurrent as CC

import Sound.File.Sndfile

type BasicOscillator = FrequencySignal -> AmplitudeSignal -> (Maybe PWMSignal) -> Signal

--------------------------------------
-- Raw Signal Generation Components
--------------------------------------

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
    basicFunc pw t  | t < pw = Cycle $ SignalValue 1
                    | otherwise = Cycle $ SignalValue (-1)

osc_triangle = oscillator basicFunc where
    basicFunc pw t   | t < pw    = (cycleFunc signalValueFromSlope t upslope) -: (Cycle $ SignalValue 1 )
                     | otherwise = (cycleFunc signalValueFromSlope (t -: pw) downslope) +: (Cycle $ SignalValue 1 )
        where
            upslope = cycleFunc getSlope pw (Cycle $ SignalValue 2)
            downslope = cycleFunc getSlope ((Cycle $ Progression 1) -: pw) (Cycle $ SignalValue (-2) )

osc_sawtooth :: BasicOscillator
osc_sawtooth fSig aSig _ = osc_triangle fSig aSig $ Just (specialize $ flatSignal 1)

osc_sine = oscillator basicFunc where
    basicFunc _ (Cycle (Progression t)) = Cycle $ SignalValue $ sin (2 * pi * t)






--------------------------------------
-- Basic Signal Manipulation Components
--------------------------------------


sig_adder :: [Signal] -> Signal
sig_adder insignals = toSignal outvalues where
    invalues = map fromSignal insignals

    -- transpose will automatically shrink the resultant lists as signals end. and sum of an empty list is safely zero
    -- in other words, any signals that go through sig_adder, we don't need to worry about them ending. saves a lot of headache.
    outvalues = map sum $ transpose invalues 


sig_sequence :: [([Signal], Progression)] -> Signal
sig_sequence sequenceData = sig_sequence' sequenceData [flatSignal 0] where
    sig_sequence' :: [([Signal], Progression)] -> [Signal] -> Signal
    sig_sequence' [] existingSignals = sig_adder existingSignals
    sig_sequence' ((newSignals, startingDelay):nextSeq) existingSignals = catSignals [beforeNewSignals, afterNewSignals] where
        beforeNewSignals = (takeSeconds startingSeconds $ sig_adder existingSignals) 
        afterNewSignals  = sig_sequence' nextSeq ( remainingOldSignals ++ newSignals ) where
            remainingOldSignals = clearEmptySignals $ map (dropSeconds startingSeconds) existingSignals
        Progression startingSeconds = startingDelay

envelope :: ([(SafeValue, Float)] -> Float -> SafeValue) -> [(SafeValue, Float)]  -> Signal
envelope envFunc points = toSignal $ envelope_ points 0 where
    envelope_ points t  | t < (len * samplesPerSecond) = (envFunc points t): envelope_ points (t + 1)
                        | otherwise = envelope_ (tail points ) 0
        where
            (val, len):(next_val, _):_ = points

slideEnvelope = envelope func where
    func points t = (val + (t * slope))
        where
            (val, len):(next_val, _):_ = points
            slope = (next_val - val) / (len * samplesPerSecond)

stepEnvelope = envelope func where
    func points _ = val
        where
            (val, len):_ = points






--------------------------------------
-- Sequencing Components
--------------------------------------











--------------------------------------
-- Sound Output Components
--------------------------------------

buffersize = 1000

outputSound s [] = do
    return ()
outputSound s signal = do
    let !buffer = take buffersize signal
    let rest = drop buffersize signal
    CC.forkIO ( simpleWrite s buffer ) >> do 
        outputSound s rest

play :: SoundSignal -> IO ()
play soundSignal = do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    outputSound s (sanitize soundSignal)
    simpleDrain s
    simpleFree s


fileinfo = Info {frames = 1000, samplerate = 44100, channels = 1, seekable = False, format=Format {headerFormat =SF.HeaderFormatWav, sampleFormat = SF.SampleFormatPcm16, endianFormat = SF.EndianLittle }, sections = 1  } 

writeSound :: SoundSignal -> FilePath -> IO ()
writeSound signal outPath = do
    SF.writeFile fileinfo outPath $ BV.toBuffer $ V.fromList $ sanitize signal
    return ()

