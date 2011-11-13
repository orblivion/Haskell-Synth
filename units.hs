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
    Progression :: (Domain domain) =>  domain -> Float -> Progression domain 

(+:) :: Progression domain -> Progression domain -> Progression domain
(+:) (Progression domain a) (Progression _ b) = Progression domain (a + b)


data Conversion domain perDomain where
    Conversion :: (Domain domain, Domain perDomain) => domain -> perDomain -> Float -> Conversion domain perDomain

(*:) :: Progression domain_a -> Conversion domain_a domain_b -> Progression domain_b
(*:) (Progression domain_a a) (Conversion _ domain_b f) = Progression domain_b (a * f)

type Second = Progression SecondDomain
type Sample = Progression SampleDomain
type Cycle  = Progression CycleDomain

type Frequency = Conversion Cycle Second
type SamplingRate = Conversion Sample Second

a = (Progression SampleDomain 5) +: (Progression SampleDomain 5)



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

