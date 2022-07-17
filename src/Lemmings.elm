module Lemmings exposing (..)


import Basics.Extra exposing (uncurry, flip)
import Bridge
import GFXAsset
import Misc
import Ocean

type alias Position = (Float, Float)
type alias RemainingTime = Float

type Impulse
  = FleeWave
  | FleeStorm
  | Pray

type Lemming
  = Living Position Impulse
    --{ position : Position }
  | Rescued Position
  | Falling Position RemainingTime
  | Dead
newLemming = Living (0, 150) FleeStorm
newPopulation = 
  List.repeat 99 newLemming
  |> List.indexedMap (\idx -> updateX (\x -> idx |> toFloat |> (+) 0.1 |> (flip (/)) 10 |> (+) x))

tossLemming lemming = 
  case lemming of 
    Living (x, y) _ -> Falling (x, y) 300
    _ -> lemming

xPos lemming =
  case lemming of
    Living (x, _) _ -> x
    Falling (x, _) _ -> x
    Dead -> 0
    Rescued (x, _) -> x

yPos lemming =
  case lemming of
    Living (_, y) _ -> y
    Falling (_, y) _ -> y
    Dead -> 0
    Rescued (_, y) -> y

isAlive lemming =
  case lemming of
    Living _ _ -> True
    Rescued _ -> True
    _ -> False

isPraying lemming =
  case impulse lemming of
    Just Pray -> True
    _ -> False

hasBeenRescued lemming =
  case lemming of
    Rescued _ -> True
    _ -> False

isFleeingStorm lemming =
  case impulse lemming of
    Just FleeStorm -> True
    _ -> False
  
isFleeingWave lemming =
  case impulse lemming of
    Just FleeWave -> True
    _ -> False

isRunning lemming =
  isFleeingStorm lemming || isFleeingWave lemming

anyLeftToRescue =
  List.any (not << hasBeenRescued)

impulse lemming =
  case lemming of
    Living _ i -> Just i
    _ -> Nothing

updateX updater lemming =
  case lemming of
    Living (x, y) i -> Living (updater x, y) i
    Falling (x, y) rt -> Falling (updater x, y) rt
    _ -> lemming

updateY updater lemming =
  case lemming of
    Living (x, y) i -> Living (x, updater y) i
    Falling (x, y) rt -> Falling (x, updater y) rt
    _ -> lemming

updateImpulse updater lemming =
  case lemming of
    Living (x, y) i -> Living (x, y) (updater i)
    _ -> lemming

updateRemainingTime updater lemming =
  case lemming of
    Falling (x, y) rt -> Falling (x, y) (updater rt)
    _ -> lemming

updateFalling delta lemming =
  case lemming of
    Falling (x, y) rt ->
      if rt - delta <= 0 then Dead else Falling (x, y + (delta/100)) (rt - delta)
    _ -> lemming

runSpeed : Float -> Impulse -> Float
runSpeed x i =
  case i of
    FleeStorm -> 2
    FleeWave -> if x > 500 then 2 else -2
    _ -> 0

makeDecisions bridge ocean delta lemming =
  let bmax = Bridge.terminus bridge
      shouldStopRunning = isFleeingStorm lemming && Misc.within (bmax - 1.5) bmax (xPos lemming)
      (dzx1, dzx2) = Ocean.waveBridgeDangerZone ocean
      shouldStartFleeingWave = Misc.within dzx1 dzx2 (xPos lemming) && (Ocean.oceanWave ocean |> Ocean.waveValue) <= 10
      canContinueCrossing = xPos lemming <= (bmax - 1.5)
  in
  if xPos lemming > bmax then 
    lemming {- |> Debug.log ("tossed") -} |> tossLemming 
  else if shouldStopRunning then
    updateImpulse (always Pray) lemming
  else if shouldStartFleeingWave then
    updateImpulse (always FleeWave) lemming
  else if canContinueCrossing then
    updateImpulse (always FleeStorm) lemming
  else lemming
  

run bridge ocean delta lemming =
  impulse lemming
  |> Maybe.map (\i -> updateX ((+) (runSpeed (xPos lemming) i * delta/100)) lemming)
  |> Maybe.map (makeDecisions bridge ocean delta)
  |> Maybe.withDefault lemming


viewLemming =
  Misc.duple
  >> Tuple.mapBoth xPos yPos
  >> uncurry GFXAsset.lemmingSprite

view =
  List.map viewLemming

updateLemming bridge ocean delta lemming =
  case lemming of
    Living (x, y) i -> run bridge ocean delta lemming
    Falling _ _ -> updateFalling delta lemming
    _ -> lemming
  

update bridge ocean delta =
  List.map (updateLemming bridge ocean delta)
  >> List.filter isAlive
