import Sound.Pulse.Simple

main=do
    s<-simpleNew Nothing "example" Play Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    simpleWrite s ([sin $ 2*pi*440*(t/44100)|t<-[1..44100*10]] :: [Float])
    simpleDrain s
    simpleFree s
