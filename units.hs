{-# LANGUAGE
    GADTs, 
    MultiParamTypeClasses, 
    FunctionalDependencies
    #-}

-- The nature of units
class Unit u where
    __dummy :: u -> u 
    __ :: SafeValue -> (UnitValue u)
    __ = UnitValue

type SafeValue = Float -- This will be a re-definition when we bring it back to the other code, so remove it then.

data UnitValue unit_type where
    UnitValue :: (Unit unit_type) => SafeValue -> UnitValue unit_type


-- The nature of sorts of units

-- Order of operations
infixr 6 +:
infixr 7 *:
infixr 7 /:

-- Starting off with Progressions, with (+:), but we can have various functions over time that operate on Progressions,
-- since they'd be used as a marker of how far along a list of items (samples, etc).
class (Unit u) => Progression u where
    __dummy2 :: u -> u

-- Not implemented as a typeclass function because it works on Units the same way
(+:) :: (Progression unit_type) => UnitValue unit_type -> UnitValue unit_type -> UnitValue unit_type
(+:) (UnitValue a) (UnitValue b) = UnitValue (a + b)

-- Next with how units interoperate.
class (Unit top, Unit bottom, Unit result) => UnitRelationship top bottom result | top bottom -> result where
    (*:) :: UnitValue bottom -> UnitValue result -> UnitValue top
    (*:) (UnitValue bottomval) (UnitValue resultval) = UnitValue (bottomval * resultval)
    (/:) :: UnitValue top -> UnitValue bottom -> UnitValue result 
    (/:) (UnitValue topval) (UnitValue bottomval) = UnitValue (topval / bottomval)


-- I was hoping the following would work, doesn't seem to:
-- Requires UndecidableInstances and OverlappingInstances, in case I try to pick it up again. Any ideas out there?
-- Basically I'm trying to implement the commutative property of multiplication, and a=b/c -> c=b/a
--instance (UnitRelationship (UnitValue top) (UnitValue bottom) (UnitValue result)) => UnitRelationship (UnitValue top) (UnitValue result) (UnitValue bottom)

-- Actual unit types and their interactions:

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

instance Unit Hertz
instance Unit SamplePerSecond
instance Unit Sample
instance Unit Cycle     -- Not a unit in the sense of physics. One cycle represents start to finish of a sine wave in a timeless domain
instance Unit Second
instance Unit Amplitude -- Not a unit in the sense of physics. One amplitude represents the ability to transform a SignalValue
instance Unit SignalValue -- Definitely not a unit in the sense of physics. We (explicitly) break unit laws by converting to other units.
instance Unit SignalSlope

instance Progression Cycle
instance Progression Second
instance Progression Sample


instance UnitRelationship Cycle Second Hertz
instance UnitRelationship Cycle Hertz Second -- lame that I have to do the commutative manually. See "hoping this would work" above.

instance UnitRelationship Sample Second SamplePerSecond
instance UnitRelationship Sample SamplePerSecond Second

instance UnitRelationship SignalValue Sample SignalSlope
instance UnitRelationship SignalValue SignalSlope Sample

instance UnitRelationship Amplitude SignalValue SignalValue -- This will make sure Amplitude inputs are used correctly.

get_frequency :: UnitValue Second -> UnitValue Cycle -> UnitValue Hertz
get_frequency s c = c /: s 

get_seconds :: UnitValue Hertz -> UnitValue Cycle -> UnitValue Second
get_seconds h c = c /: h

add_5_seconds s = (s :: (UnitValue Second)) +: _second 5
