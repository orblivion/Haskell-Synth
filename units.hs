class Domain d where
    dummy1 :: d -> d
    dummy1 x = x

class Range r where
    dummy2 :: r -> r
    dummy2 x = x

data SecondDomain   = SecondDomain
instance Domain SecondDomain

data CycleDomain    = CycleDomain
instance Domain CycleDomain

data SampleDomain   = SampleDomain
instance Domain SampleDomain


data Amplitude = Amplitude
instance Range Amplitude


data Progression domain where
    Progression :: (Domain domain) =>  Float -> Progression domain 

(+:) :: Progression domain -> Progression domain -> Progression domain
(+:) (Progression a) (Progression b) = Progression (a + b)


data Conversion domain perDomain where
    Conversion :: (Domain domain, Domain perDomain) => Float -> Conversion domain perDomain

(*:) :: Progression domain_a -> Conversion domain_b domain_a -> Progression domain_b
(*:) (Progression a) (Conversion f) = Progression (a * f)

type Second = Progression SecondDomain
second :: Float -> Second
second a = Progression a

type Sample = Progression SampleDomain
type Cycle  = Progression CycleDomain

type Frequency = Conversion Cycle Second
frequency :: Float -> Frequency
frequency a = Conversion a

type SamplingRate = Conversion Sample Second


a = ( (second 5) +: (second 5) ) *: ( frequency 20 )



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

