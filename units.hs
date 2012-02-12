{-# LANGUAGE
    GADTs, 
    MultiParamTypeClasses, 
    TypeSynonymInstances, 
    FunctionalDependencies
    #-}

module Units where

-- The nature of units
class Unit unit_type num_type | unit_type -> num_type where
    __dummy :: unit_type -> unit_type 
    __ :: num_type -> (UnitValue unit_type num_type)
    __ = UnitValue

type SafeValue = Float -- This will be a re-definition when we bring it back to the other code, so remove it then.

class (Num num_type) => UnitNum num_type where
    toSafeValue :: num_type -> SafeValue
    fromSafeValue :: SafeValue -> num_type

instance UnitNum SafeValue where
    toSafeValue num = num
    fromSafeValue num = num

instance UnitNum Integer where
    toSafeValue num = fromIntegral num
    fromSafeValue num = floor num

data UnitValue unit_type num_type where
    UnitValue :: (Unit unit_type num_type) => num_type -> UnitValue unit_type num_type


-- The nature of sorts of units

-- Order of operations
infixr 6 +:
infixr 7 *:
infixr 7 /:

-- Starting off with Progressions, with (+:), but we can have various functions over time that operate on Progressions,
-- since they'd be used as a marker of how far along a list of items (samples, etc).
class (Unit u num_type) => Progression u num_type where
    __dummy2 :: u -> u

-- Not implemented as a typeclass function because it works on Units the same way
(+:) :: (Progression unit_type num_type) => UnitValue unit_type num_type-> UnitValue unit_type num_type -> UnitValue unit_type num_type
(+:) (UnitValue a) (UnitValue b) = UnitValue (a + b)

-- Next with how units interoperate.
class (UnitNum t_num, UnitNum b_num, UnitNum r_num, Unit top t_num, Unit bottom b_num, Unit result r_num)
    => UnitRelationship top bottom result t_num b_num r_num | top bottom -> result, top -> t_num, bottom -> b_num, result -> r_num where
    (*:) :: UnitValue bottom b_num -> UnitValue result r_num -> UnitValue top t_num
    (*:) (UnitValue bottomval) (UnitValue resultval) = UnitValue (bottomval * resultval)
    (/:) :: UnitValue top t_num -> UnitValue bottom b_num -> UnitValue result r_num
    (/:) (UnitValue topval) (UnitValue bottomval) = UnitValue (topval / bottomval)


-- I was hoping the following would work, doesn't seem to:
-- Requires UndecidableInstances and OverlappingInstances, in case I try to pick it up again. Any ideas out there?
-- Basically I'm trying to implement the commutative property of multiplication, and a=b/c -> c=b/a
--instance (UnitRelationship (UnitValue top) (UnitValue bottom) (UnitValue result)) => UnitRelationship (UnitValue top) (UnitValue result) (UnitValue bottom)

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
_second      = __ :: SafeValue -> UnitValue Second
_cycle       = __ :: SafeValue -> UnitValue Cycle 
_hertz       = __ :: SafeValue -> UnitValue Hertz
_amplitude   = __ :: SafeValue -> UnitValue Amplitude
_signalvalue = __ :: SafeValue -> UnitValue SignalValue

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
 -- lame that I have to do the commutative manually. See "hoping this would work" above.
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
