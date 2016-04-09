module Signals where

samplesPerSecond :: (Num a) => a
samplesPerSecond = 44100

type SafeValue = Float
data SignalValue = SignalValue SafeValue
data Signal = Signal [SignalValue]

flatSignal val = toSignal [val, val..]

-- Need to make this be ok with signals that end pretty soon or I'll be in trouble.
takeSeconds s (Signal sigdata) = Signal $ take (floor (s * samplesPerSecond)) sigdata
dropSeconds s (Signal sigdata) = Signal $ drop (floor (s * samplesPerSecond)) sigdata

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
        sanitize (SignalValue sigvalue)  = sigvalue
-- cut this out for now because of LFOs. maybe make that type carry over n shit
--        sanitize (SignalValue sigvalue)   | sigvalue < 0 = 0
--                            | sigvalue > 1 = 1
--                            | otherwise = sigvalue


data PWMSignal = PWMSignal [SignalValue]

instance SpecializedSignal PWMSignal where
    specialize (Signal sigvalues) = PWMSignal sigvalues
    sanitize (PWMSignal sigvalues) = map sanitize sigvalues where
        sanitize (SignalValue sigvalue)   | sigvalue < -1 = -1
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

catSignals :: [Signal] -> Signal
catSignals sigs = toSignal $ concat $ (map fromSignal sigs) where


clearEmptySignals signals = signals -- eventually as a memory saving technique I want to use this. for now it's just a reminder.
