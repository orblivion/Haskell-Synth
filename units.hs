{-# LANGUAGE
    GADTs, 
    MultiParamTypeClasses, 
    TypeSynonymInstances, 
    FunctionalDependencies,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances
    #-}



module Units where

-- The nature of units
class (Num num_type) => Unit unit_type num_type | unit_type -> num_type where
    __ :: num_type -> (UnitValue unit_type num_type)
    __ = UnitValue

type SafeValue = Float -- This will be a re-definition when we bring it back to the other code, so remove it then.

data UnitValue unit_type num_type where
    UnitValue :: (Unit unit_type num_type, Num num_type) => num_type -> UnitValue unit_type num_type


-- The nature of sorts of units

-- Order of operations
infixr 6 +:
infixr 7 *:
infixr 7 /:

-- Starting off with Progressions, with (+:), but we can have various functions over time that operate on Progressions,
-- since they'd be used as a marker of how far along a list of items (samples, etc).
class (Unit u num_type) => Progression u num_type where
    __dummy :: u -> u

-- Not implemented as a typeclass function because it works on Units the same way
(+:) :: (Progression unit_type num_type) => UnitValue unit_type num_type-> UnitValue unit_type num_type -> UnitValue unit_type num_type
(+:) (UnitValue a) (UnitValue b) = UnitValue (a + b)

-- Next with how units interoperate.
class (Num t_num, Num b_num, Num r_num, Unit top t_num, Unit bottom b_num, Unit result r_num, UnitRelationshipDefault t_num b_num r_num)
    => UnitRelationship top bottom result t_num b_num r_num | top bottom -> result, top -> t_num, bottom -> b_num, result -> r_num where
    (*:) :: UnitValue bottom b_num -> UnitValue result r_num -> UnitValue top t_num
    (*:) (UnitValue a) (UnitValue b) = UnitValue $ default_mult a b
    (/:) :: UnitValue top t_num -> UnitValue bottom b_num -> UnitValue result r_num
    (/:) (UnitValue a) (UnitValue b) = UnitValue $ default_div a b

class (Num t_num, Num b_num, Num r_num) => UnitRelationshipDefault t_num b_num r_num where
    default_mult :: b_num -> r_num -> t_num
    default_div  :: t_num -> b_num -> r_num

instance UnitRelationshipDefault SafeValue SafeValue SafeValue where
    default_mult b r = b * r
    default_div  t b = t / b

instance UnitRelationshipDefault SafeValue SafeValue Integer where
    default_mult b r = b * (fromIntegral r)
    default_div  t b = floor $ t / b

instance UnitRelationshipDefault SafeValue Integer SafeValue where
    default_mult b r = (fromIntegral b) * r
    default_div  t b = t / (fromIntegral b)

instance UnitRelationshipDefault Integer Integer SafeValue where
    default_mult b r = floor $ (fromIntegral b) * r
    default_div  t b = (fromIntegral t) / (fromIntegral b)

instance UnitRelationshipDefault Integer SafeValue Integer where
    default_mult b r = 5 -- floor $ b * (fromIntegral r)
    default_div  t b = 5 -- floor $ (fromIntegral t) /  b


-- Actual unit types and their interactions:

-- (Notes)
-- TimeSamplingRate = TimeSamples/Time
-- Frequency = NumCycles/Time
-- Time * Frequency = NumCycles
-- Time * SamplingRate = Samples
-- CycleSamplingRate = CycleSamples/NumCycles


data Hertz = Hertz 
data Cycle = Cycle
data Second = Second
data Sample = Sample
data Amplitude = Amplitude
data SignalValue = SignalValue
data SignalSlope = SignalSlope
data SamplePerSecond = SamplePerSecond

-- Some shortcuts, since the type of __ aka UnitValue is ambiguous, and Hertz etc are only a parameter.
-- Would be nice if these were automatically made
_second      = __ :: SafeValue -> UnitValue Second SafeValue
_sample      = __ :: Integer   -> UnitValue Sample Integer
_cycle       = __ :: SafeValue -> UnitValue Cycle SafeValue
_hertz       = __ :: SafeValue -> UnitValue Hertz SafeValue
_amplitude   = __ :: SafeValue -> UnitValue Amplitude SafeValue
_signalvalue = __ :: SafeValue -> UnitValue SignalValue SafeValue

instance Unit Hertz SafeValue
instance Unit SamplePerSecond Integer
instance Unit Sample Integer

-- Not a unit in the sense of physics. One cycle represents start to finish of a sine wave in a timeless domain
instance Unit Cycle SafeValue
instance Unit Second SafeValue
instance Unit Amplitude SafeValue -- Not a unit in the sense of physics. One amplitude represents the ability to transform a SignalValue

-- Definitely not a unit in the sense of physics. We (explicitly) break unit laws by converting to other units.
instance Unit SignalValue SafeValue
instance Unit SignalSlope SafeValue

instance Progression Cycle SafeValue 
instance Progression Second SafeValue
instance Progression Sample Integer


instance UnitRelationship Cycle Second Hertz SafeValue SafeValue SafeValue
 -- sortof lame that I have to do the commutative manually, but I actually don't want it
 -- automatically implied anyway. for instance, I see no reason (yet) to end up with
 -- a sampling rate as a result, that should actually be constant
instance UnitRelationship Cycle Hertz Second SafeValue SafeValue SafeValue

instance UnitRelationship Sample Second SamplePerSecond Integer SafeValue Integer
instance UnitRelationship Sample SamplePerSecond Second Integer Integer SafeValue

instance UnitRelationship SignalValue Sample SignalSlope SafeValue Integer SafeValue
instance UnitRelationship SignalValue SignalSlope Sample SafeValue SafeValue Integer

-- This will make sure Amplitude inputs are used correctly.
instance UnitRelationship Amplitude SignalValue SignalValue SafeValue SafeValue SafeValue

get_frequency :: UnitValue Second SafeValue -> UnitValue Cycle SafeValue -> UnitValue Hertz SafeValue
get_frequency s c = c /: s 

get_seconds :: UnitValue Hertz SafeValue -> UnitValue Cycle SafeValue -> UnitValue Second SafeValue
get_seconds h c = c /: h

add_5_seconds s = (s :: (UnitValue Second SafeValue)) +: _second 5
