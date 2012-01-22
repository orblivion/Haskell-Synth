-- The nature of units
class Unit u where
    __ :: Float -> u

class Domain d where
    __dummy1 :: d -> d
    __dummy1 x = x

class Range r where
    __dummy2 :: r -> r
    __dummy2 x = x



-- The nature of domains, ranges, and conversions.

data Progression domain where
    Progression :: (Domain domain) =>  Float -> Progression domain 

data Conversion domain perDomain where
    Conversion :: (Domain domain, Domain perDomain) => Float -> Conversion domain perDomain

instance (Domain domain) => Unit (Progression domain) where
    __ n = Progression n

--instance (Range range) => Unit (Range range) where
--    __ n = Progression n

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

