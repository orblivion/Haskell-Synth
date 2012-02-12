{-# LANGUAGE
    GADTs, 
    MultiParamTypeClasses, 
    FunctionalDependencies,
    FlexibleInstances,
    UndecidableInstances
    #-}


-- Here's the overall plan:

-- Similarly, SignalType can specify what can come out of a component. Be it a frequency, or whatnot. Or maybe we don't want to play favorites with what can go into what... hmm. Maybe for InputTypes then.

-- And instead of Cycle Amplitude, it'll be something like BaseAmplitude
-- "Range", or "Value", I suppose, typeclass. For amplitude n stuff. But only maybe, just something to consider.

-- The nature of units
class Unit u where
    __dummy :: u -> u 
    __ :: SafeValue -> (UnitValue u)
    __ = UnitValue

type SafeValue = Float -- This will be a re-definition when we bring it back to the other code, so remove it then.

data UnitValue unit_type where
    UnitValue :: (Unit unit_type) => SafeValue -> UnitValue unit_type


-- The nature of sorts of units

-- Starting off with (+:), but We can have various functions over time that operate on Progressions,
-- since they'd be used as a marker of how far along a list of items (samples, etc).
class (Unit u) => Progression u where
    __dummy2 :: u -> u

infixr 6 +:
infixr 7 *:
infixr 7 /:

-- Not implemented as a typeclass function because it works on Units the same way
(+:) :: (Progression unit_type) => UnitValue unit_type -> UnitValue unit_type -> UnitValue unit_type
(+:) (UnitValue a) (UnitValue b) = UnitValue (a + b)

class (Unit top, Unit bottom, Unit result) => UnitRelationship top bottom result | top bottom -> result where
    (*:) :: UnitValue bottom -> UnitValue result -> UnitValue top
    (*:) (UnitValue bottomval) (UnitValue resultval) = UnitValue (bottomval * resultval)
    (/:) :: UnitValue top -> UnitValue bottom -> UnitValue result 
    (/:) (UnitValue topval) (UnitValue bottomval) = UnitValue (topval / bottomval)

instance (UnitRelationship top bottom result) => UnitRelationship top result bottom

data Hertz = Hertz 
data Cycle = Cycle
data Second = Second
data Amplitude = Amplitude
data SignalValue = SignalValue

_second      = __ :: SafeValue -> UnitValue Second
_cycle       = __ :: SafeValue -> UnitValue Cycle 
_hertz       = __ :: SafeValue -> UnitValue Hertz
_amplitude   = __ :: SafeValue -> UnitValue Amplitude
_signalvalue = __ :: SafeValue -> UnitValue SignalValue

instance Unit Hertz
instance Unit Cycle     -- Not a unit in the sense of physics. One cycle represents
instance Unit Second
instance Unit Amplitude -- Not a unit in the sense of physics
instance Unit SignalValue -- Definitely not a unit in the sense of physics

instance Progression Cycle
instance Progression Second

instance UnitRelationship Cycle Second Hertz
instance UnitRelationship Amplitude SignalValue SignalValue -- This will make sure Amplitude inputs are used correctly.

-- This is a signal type. It can convert to errethang. But only explicitly!
-- Change this to SignalValue
--fromGeneric :: UnitValue Generic -> UnitValue 
--fromGeneric g val = __ 

get_frequency :: UnitValue Second -> UnitValue Cycle -> UnitValue Hertz
get_frequency s c = c /: s 

add_5_seconds s = (s :: (UnitValue Second)) +: _second 5

{-
------------

data Conversion domain perDomain where
    Conversion :: (Domain domain, Domain perDomain) => Float -> Conversion domain perDomain

instance (Domain domain) => Unit (Progression domain) where
    __ n = Progression n

instance (Range range) => Unit (!!! Hello range) where
    __ n = Progression n

instance (Domain domain, Domain perDomain) => Unit (Conversion domain perDomain) where
    __ n = Conversion n




-- Things that domains, ranges, and conversions can do

(+:) :: Progression domain -> Progression domain -> Progression domain
(+:) (Progression a) (Progression b) = Progression (a + b)


-- It has been pointed out to me that this, as it stands, offers me no type safety.
(*:) :: Progression domain_a -> Conversion domain_b domain_a -> Progression domain_b
(*:) (Progression a) (Conversion f) = Progression (a * f)




-- Some specific domains and ranges

data SecondDomain   = SecondDomain
instance Domain SecondDomain
type Second = Progression SecondDomain
second :: Float -> Second
second a = Progression a

data CycleDomain    = CycleDomain
instance Domain CycleDomain
type Cycle = Progression CycleDomain
cycle :: Float -> Cycle 
cycle a = Progression a

data SampleDomain   = SampleDomain
instance Domain SampleDomain
type Sample = Progression SampleDomain
sample :: Float -> Sample
sample a = Progression a

type Frequency = Conversion CycleDomain SecondDomain
frequency :: Float -> Frequency
frequency a = Conversion a

type SamplingRate = Conversion SampleDomain SecondDomain
samplingrate :: Float -> SamplingRate 
samplingrate a = Conversion a


--data AmplitudeRange = AmplitudeRange
--instance Range Amplitude



a :: Cycle
a =  second 5 +: __ 5  *: __ 20



--data Progression    = Progression   deriving Aspect
--data Slope          = Slope         deriving Aspect
--data SamplingRate   = SamplingRate  deriving Aspect
--data Frequency      = Frequency     deriving Aspect


--SamplingRate

--Progression




-- TimeSamplingRate = TimeSamples/Time
-- Frequency = NumCycles/Time
-- Time * Frequency = NumCycles
-- Time * SamplingRate = Samples
-- CycleSamplingRate = CycleSamples/NumCycles

-}
