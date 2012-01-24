-- Here's the overall plan:

-- Progression is a sub-typeclass of Unit, which implements (+) on those Units. We can have various functions, then, that operate on Progressions, since they'd be used as a marker of how far along a list of items.
-- Similarly, SignalType can specify what can come out of a component. Be it a frequency, or whatnot. Or maybe we don't want to play favorites with what can go into what... hmm. Maybe for InputTypes then.

-- Conversion is an intermediary object, or perhaps just an intermediary type that never gets instantiated, which through some type magic (GADT specifying that it has to be of the Convertible typeclass), facilitates *: between only certain things.
-- Conversion Unit Unit Unit - unit, unitFrom, unitTo, 

-- (:*) Gets implemented with something like this:
-- (:*) :: (UnitValue ut1) -> (UnitValue ut2) -> (UnitValue ut3)
-- (:*) (UnitValue u1 v1) (UnitType u2 v2) = (UnitType u3 (v1 * v2)) where
--     conversion :: Conversion u1 u2 u3 -- don't do anything with this, just make an error if this is invalid

-- And instead of Cycle Amplitude, it'll be something like BaseAmplitude
   

-- The nature of units
class Unit unit_type where
    __dummy :: a -> a

type SafeValue = Float -- This will be a re-definition when we bring it back to the other code, so remove it then.

type UnitValue u where
    UnitValue :: (Unit unit_type) => SafeValue -> UnitValue unit_type

__ = UnitValue


-- The nature of sorts of units

class (Unit u) => Progression (Unit u) where
    __dummy2 :: a -> a

-- Not implemented as a typeclass function because it only needs to be implemented once
(+:) :: (Progression unit_type) => Unit unit_type -> Unit unit_type -> Unit unit_type
(+:) (Unit a) (Unit b) = Unit (a + b)

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

