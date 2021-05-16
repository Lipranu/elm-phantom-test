module Main exposing (..)

import Internal exposing (..)

type alias SideInitial side =
    { inputT : Temperature side Input
    , outputT : Temperature side Output
    }

type alias Initial =
    { hot : SideInitial Hot
    , cold : SideInitial Cold
    , power : Power
    }

type alias SideResult side =
    { mean : Temperature side Mean
    , massFlow : MassFlow side
    , capacity : Capacity side
    }

type alias Result =
    { hot : SideResult Hot
    , cold : SideResult Cold
    , wall : Temperature General Wall
    , lmtd : Temperature General Lmtd
    }

result : Initial -> Result
result i =
    let gSide = calcSide i.power
        hot = gSide i.hot
        cold = gSide i.cold
    in  { hot = hot
        , cold = cold
        , wall = wallT hot.mean cold.mean
        , lmtd = lmtd i.hot.inputT i.hot.outputT i.cold.inputT i.cold.outputT
        }

calcSide : Power -> SideInitial side -> SideResult side
calcSide p s =
    let mean = meanT s.inputT s.outputT
        cap  = capacity mean
        mass = massFlow p cap s.inputT s.outputT
    in { massFlow = mass, mean = mean, capacity = cap }

thi : Temperature Hot Input
thi = temperature 100

tho : Temperature Hot Output
tho = temperature 70

tci : Temperature Cold Input
tci = temperature 55

tco : Temperature Cold Output
tco = temperature 80

pow : Power
pow = power 300

init : Initial
init =
    let initH = { inputT = temperature 130, outputT = temperature 70 }
        initC = { inputT = temperature 50, outputT = temperature 90 }
     in { hot = initH, cold = initC, power = power 440 }
