module Internal exposing
    ( Cold
    , Hot
    , General

    , Input
    , Output
    , Mean
    , Wall
    , Lmtd

    , Temperature
    , MassFlow
    , Power
    , Capacity

    , temperature
    , power
    , capacity
    , massFlow
    , meanT
    , wallT
    , lmtd
    )

type Cold = Cold
type Hot = Hot
type General = General

type Input = Input
type Output = Output
type Mean = Mean
type Wall = Wall
type Lmtd = Lmtd

type Temperature side role = Temperature Float

temperature : Float -> Temperature side role
temperature = Temperature

type Power = Power Float

power : Float -> Power
power = Power

type Capacity side = Capacity Float

capacity : Temperature side Mean -> Capacity side
capacity (Temperature t) = Capacity <| if t > 100 then 4.21 else 4.2

type MassFlow side = MassFlow Float

massFlow : Power
    -> Capacity side
    -> Temperature side Input
    -> Temperature side Output
    -> MassFlow side
massFlow (Power p) (Capacity c) (Temperature ti) (Temperature to) =
    let maxT = max ti to
        minT = min ti to
        diffT = maxT - minT
    in MassFlow <| p / c / diffT

meanT : Temperature side Input
    -> Temperature side Output
    -> Temperature side Mean
meanT = tempHelper

wallT : Temperature Hot Mean
    -> Temperature Cold Mean
    -> Temperature General Wall
wallT = tempHelper

tempHelper : Temperature side1 role1
    -> Temperature side2 role2
    -> Temperature side3 role3
tempHelper (Temperature t1) (Temperature t2) = Temperature <| (t1 + t2) / 2

lmtd : Temperature Hot Input
    -> Temperature Hot Output
    -> Temperature Cold Input
    -> Temperature Cold Output
    -> Temperature General Lmtd
lmtd (Temperature thi) (Temperature tho) (Temperature tci) (Temperature tco) =
    let t1 = thi - tco
        t2 = tho - tci
        top = t1 - t2
        bottom = logBase e <| t1 / t2
    in Temperature <| if t1 == t2 then t1 else top / bottom
