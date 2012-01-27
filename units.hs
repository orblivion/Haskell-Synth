-- Here's the overall plan:

-- Similarly, SignalType can specify what can come out of a component. Be it a frequency, or whatnot. Or maybe we don't want to play favorites with what can go into what... hmm. Maybe for InputTypes then.

-- And instead of Cycle Amplitude, it'll be something like BaseAmplitude
-- "Range", or "Value", I suppose, typeclass. For amplitude n stuff. But only maybe, just something to consider.

-- The nature of units
class Unit u where
    __dummy :: u -> u 

type SafeValue = Float -- This will be a re-definition when we bring it back to the other code, so remove it then.

data UnitValue unit_type where
    UnitValue :: (Unit unit_type) => SafeValue -> UnitValue unit_type

__ = UnitValue


-- The nature of sorts of units

-- Starting off with (+:), but We can have various functions over time that operate on Progressions,
-- since they'd be used as a marker of how far along a list of items (samples, etc).
class (Unit u) => Progression u where
    __dummy2 :: u -> u

-- Not implemented as a typeclass function because it works on Units the same way
(+:) :: (Progression unit_type) => UnitValue unit_type -> UnitValue unit_type -> UnitValue unit_type
(+:) (UnitValue a) (UnitValue b) = UnitValue (a + b)

class (Unit top, Unit bottom, Unit result) => Conversion top bottom result where
    (*:) :: UnitValue bottom -> UnitValue result -> UnitValue top
    (*:) (UnitValue bottomval) (UnitValue resultval) = UnitValue (bottomval * resultval)
    (/:) :: UnitValue top -> UnitValue bottom -> UnitValue result 
    (/:) (UnitValue topval) (UnitValue bottomval) = UnitValue (topval / bottomval)

-- Conversion between units
--data Conversion unit unitTop unitBottom
--    (ConversionClass (Conversion unit unitTop unitBottom)) => Conversion unit unitTop unitBottom

data Frequency = Frequency
instance Unit (UnitValue Frequency)

data Cycle = Cycle
instance Unit (UnitValue Cycle)

data Second = Second
instance Unit (UnitValue Second)

instance Conversion Cycle Second Frequency

x :: Frequency
x = __ 5 * __ 5


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
