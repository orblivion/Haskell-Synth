import Sound.Pulse.Simple

mySin = ([tapering t * (sin $ 2*pi*440*(t/rate))|t<-[1..rate*seconds]] :: [Float]) where
    tapering t = rate / (rate + t)
    rate = 44100
    seconds = 2


main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s mySin
    simpleDrain s
    simpleFree s
