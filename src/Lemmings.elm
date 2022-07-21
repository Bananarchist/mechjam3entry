module Lemmings exposing (..)


import Basics.Extra exposing (uncurry, flip)
import Bridge
import Dict
import GFXAsset
import List.Extra
import Misc
import Maybe.Extra
import Ocean

type alias Position = (Float, Float)
type alias RemainingTime = Float

type Impulse
  = FleeWave
  | FleeStorm
  | Pray

type alias LemmingStats = Dict.Dict String Attribute

type Attribute
  = Speed Float
  | Height Float
  | Distance Float

type Lemming
  = Living LemmingStats Position Impulse
    --{ position : Position }
  | Sneaking LemmingStats Position Impulse
  | Rescued Position
  | Falling Position RemainingTime
  | Dead

defaultSpeed = 50.0
defaultHeight = 0.75 
defaultDistance = 50
newLemming statistics = newLemmingSetX 0 statistics
newLemmingSetX x statistics = Living statistics (x, 140) FleeStorm
newStats = Dict.fromList [("speed", Speed defaultSpeed), ("height", Height defaultHeight), ("distance", Distance defaultDistance)]
newPopulation = List.repeat 99 (newLemming newStats)
newPopulationWithRandomValues : List Float -> (List Float, List Lemming)
newPopulationWithRandomValues vals = 
  newPopulation
  |> flip mapRandomValues vals

{- We want to check if hasSpeedState - if not, create one with rand val
if so, then (later) we should look to other rand vals to introduce such
as mapping to risk taker (eg, tightrope walker) -}

mapRandomValues : List Lemming -> List Float -> (List Float, List Lemming)
mapRandomValues lemmings lval =
  let loop : List ((Float -> Float) -> LemmingStats -> LemmingStats) -> List Float -> List Lemming -> (List Float, List Lemming)
      loop fns vals lems =
        case fns of
          [] -> (vals, lems)
          f::fs -> 
            if List.length lems > List.length vals then
              (vals, lems)
            else
              List.take (List.length lems) vals
              |> List.Extra.zip lems
              |> List.map (\(l, s) -> mapAttrs ((f |> Debug.log "fn") (always s)) l)
              |> Tuple.pair (List.drop (List.length lems) vals |> Debug.log "Remaining vals")
              |> uncurry (loop fs)
  in
  List.take (List.length lemmings) lval
  |> List.Extra.zip lemmings
  |> List.map (\(l, s) ->  updateX (always s) l |> mapAttrs (mapDistanceStat (always (s / 3)) ))
  |> Tuple.pair (List.drop (List.length lemmings) lval |> Debug.log "Remaining vals")
  |> uncurry (loop [mapSpeedStat, mapHeightStat])


{-
hasSpeedStat : Lemming -> Bool
hasSpeedStat =
  stats 
  >> Maybe.map (List.any isSpeedAttr) 
  >> Maybe.withDefault False
-}

mapDistanceStat : (Float -> Float) -> LemmingStats -> LemmingStats
mapDistanceStat updater =
  Dict.update "distance" (Maybe.map (distanceValue >> updater >> Distance))


mapSpeedStat : (Float -> Float) -> LemmingStats -> LemmingStats
mapSpeedStat updater =
  Dict.update "speed" (Maybe.map (speedValue >> updater >> Speed))

mapHeightStat : (Float -> Float) -> LemmingStats -> LemmingStats
mapHeightStat updater =
  Dict.update "height" (Maybe.map (heightValue >> updater >> Height))

mapAttrs : (LemmingStats -> LemmingStats) -> Lemming -> Lemming
mapAttrs updater lemming =
  case lemming of
    Living attrs p i -> Living (updater attrs) p i
    _ -> lemming

isSpeedAttr attr =
  case attr of
    Speed _ -> True
    _ -> False

distanceValue attr =
  case attr of
    Distance v -> v
    _ -> defaultDistance


speedValue attr =
  case attr of
    Speed v -> v
    _ -> defaultSpeed

heightValue attr =
  case attr of
    Height v -> v
    _ -> defaultHeight
    
tossLemming lemming = 
  case lemming of 
    Living _ (x, y) _ -> Falling (x, y) 300
    Sneaking _ (x, y) _ -> Falling (x, y) 300
    _ -> lemming

xPos lemming =
  case lemming of
    Living _ (x, _) _ -> x
    Sneaking _ (x, _) _ -> x
    Falling (x, _) _ -> x
    Dead -> 0
    Rescued (x, _) -> x

yPos lemming =
  case lemming of
    Living _ (_, y) _ -> y
    Sneaking _ (_, y) _ -> y
    Falling (_, y) _ -> y
    Dead -> 0
    Rescued (_, y) -> y

stats lemming =
  case lemming of
    Living attrs _ _ -> attrs
    Sneaking attrs _ _ -> attrs
    _ -> Dict.empty 

attribute =
  Dict.get

isAlive lemming =
  case lemming of
    Living _ _ _ -> True
    Sneaking _ _ _ -> True
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

isFalling lemming = 
  case lemming of
    Falling _ _ -> True
    _ -> False



anyLeftToRescue =
  List.any (not << hasBeenRescued)

impulse lemming =
  case lemming of
    Living _ _ i -> Just i
    _ -> Nothing

updateX updater lemming =
  case lemming of
    Living s (x, y) i -> Living s (updater x, y) i
    Falling (x, y) rt -> Falling (updater x, y) rt
    _ -> lemming

updateY updater lemming =
  case lemming of
    Living s (x, y) i -> Living s (x, updater y) i
    Falling (x, y) rt -> Falling (x, updater y) rt
    _ -> lemming

updateImpulse updater lemming =
  case lemming of
    Living s (x, y) i -> Living s (x, y) (updater i)
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
 
mapStats updater lemming =
  case lemming of 
    Living s c i -> Living (updater s) c i
    _ -> lemming

clampedSpeed x =
  if x < 50 then clamp 30 70 (x + 20) else clamp 30 70 (x - 20)

runSpeed : Lemming -> Float
runSpeed lemming =
  let speedAttr = stats lemming |> attribute "speed" |> Maybe.map (speedValue >> clampedSpeed >> (*) 0.01)
  in
  impulse lemming
    |> Maybe.map (\i ->
      case i of
        FleeStorm -> 4
        FleeWave -> if xPos lemming > 300 then 4 else -4
        _ -> 0
    )
  |> flip Maybe.Extra.andMap (Maybe.map (*) speedAttr)
  |> Maybe.withDefault 0

makeDecisions bridge ocean delta lemming =
  let distance = stats lemming |> attribute "distance" |> Maybe.map distanceValue |> Maybe.withDefault defaultDistance
      bmax = Bridge.terminus bridge
      shouldStopRunning = isFleeingStorm lemming && Misc.within (bmax - 1.5 - distance) bmax (xPos lemming)
      (dzx1, dzx2) = Ocean.waveBridgeDangerZone ocean
      shouldStartFleeingWave = Misc.within dzx1 dzx2 (xPos lemming) && (Ocean.oceanWave ocean |> Ocean.waveValue) <= 10
      canContinueCrossing = xPos lemming <= (bmax - 1.5 - distance)
      willSneak = False
  in
  if Ocean.isWaveCrashing ocean && shouldStartFleeingWave then -- too late to flee now...
    lemming |> Debug.log ("tossed") |> tossLemming 
  else if willSneak && (not shouldStartFleeingWave) then
    lemming
    --Sneaking (xPos lemming, yPos lemming) (FleeingStorm)
  else if xPos lemming > 560 then 
    Rescued (xPos lemming, yPos lemming)
  else if xPos lemming > bmax then 
    lemming {- |> Debug.log ("tossed") -} |> tossLemming 
  else if shouldStopRunning then
    updateImpulse (always Pray) lemming
  else if shouldStartFleeingWave then
    updateImpulse (always FleeWave) lemming
  else if canContinueCrossing then
    updateImpulse (always FleeStorm) lemming
  else lemming
  

run bridge ocean delta lemming =
  updateX ((+) ((runSpeed lemming) * delta/100)) lemming
  |> makeDecisions bridge ocean delta


viewLemming lemming =
  stats lemming 
    |> attribute "height"
    |> Maybe.map (heightValue >> (\x -> if x < 33 then x * 3 else if x < 66 then x * 1.5 else x) >> flip (/) 100)
    |> Maybe.withDefault defaultHeight
    |> GFXAsset.lemmingSprite (xPos lemming) (yPos lemming)

view =
  List.map viewLemming

updateLemming bridge ocean delta lemming =
  case lemming of
    Living _ (x, y) i -> run bridge ocean delta lemming
    Falling _ _ -> updateFalling delta lemming
    Sneaking _ _ _ -> run bridge ocean delta lemming
    _ -> lemming
  

update bridge ocean delta =
  List.map (updateLemming bridge ocean delta)
  >> List.filter isAlive
