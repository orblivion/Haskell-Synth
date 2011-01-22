module Signals where

samplesPerSecond :: (Num a) => a
samplesPerSecond = 44100

type SafeValue = Float
data SignalValue = SignalValue SafeValue
data Signal = Signal [SignalValue]

flatSignal val = toSignal [val, val..]

takeSeconds s (Signal sigdata) = Signal $ take (s * samplesPerSecond) sigdata

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

