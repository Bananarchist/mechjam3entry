module Lemmings exposing (..)


import Basics.Extra exposing (uncurry, flip, fractionalModBy)
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
  = FleeWave Float
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
  | Falling LemmingStats Position RemainingTime
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
    Living s (x, y) _ -> Falling s (x, y) 1000
    Sneaking s (x, y) _ -> Falling s (x, y) 1000
    _ -> lemming

xPos lemming =
  case lemming of
    Living _ (x, _) _ -> x
    Sneaking _ (x, _) _ -> x
    Falling _ (x, _) _ -> x
    Dead -> 0
    Rescued (x, _) -> x

yPos lemming =
  case lemming of
    Living _ (_, y) _ -> y
    Sneaking _ (_, y) _ -> y
    Falling _ (_, y) _ -> y
    Dead -> 0
    Rescued (_, y) -> y

stats lemming =
  case lemming of
    Living attrs _ _ -> attrs
    Sneaking attrs _ _ -> attrs
    Falling attrs _ _ -> attrs
    _ -> Dict.empty 

attribute =
  Dict.get

isAlive lemming =
  case lemming of
    Living _ _ _ -> True
    Sneaking _ _ _ -> True
    Rescued _ -> True
    _ -> False

isUpdatable lemming =
  case lemming of
    Dead -> False
    _ -> True

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
    Just (FleeWave _) -> True
    _ -> False

isRunning lemming =
  isFleeingStorm lemming || isFleeingWave lemming

isFalling lemming = 
  case lemming of
    Falling _ _ _ -> True
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
    Falling s (x, y) rt -> Falling s (updater x, y) rt
    _ -> lemming

updateY updater lemming =
  case lemming of
    Living s (x, y) i -> Living s (x, updater y) i
    Falling s (x, y) rt -> Falling s (x, updater y) rt
    _ -> lemming

updateImpulse updater lemming =
  case lemming of
    Living s (x, y) i -> Living s (x, y) (updater i)
    _ -> lemming

updateRemainingTime updater lemming =
  case lemming of
    Falling s (x, y) rt -> Falling s (x, y) (updater rt)
    _ -> lemming

updateFalling delta lemming =
  case lemming of
    Falling s (x, y) rt ->
      let height = s |> attribute "height" |> Maybe.map heightValue |> Maybe.withDefault defaultHeight
          xDir = height |> fractionalModBy 2 |> (\v -> if v > 1 then 1 else -1)
          xMod = height / 90 * delta / 100 * xDir
      in
      if rt - delta <= 0 then Dead else Falling s (x + xMod, y + (delta/10)) (rt - delta)
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
        FleeStorm -> 3.5
        FleeWave n -> n * 3.5 --if xPos lemming > 300 then 3.5 else -3.5
        _ -> 0
    )
  |> flip Maybe.Extra.andMap (Maybe.map (*) speedAttr)
  |> Maybe.withDefault 0

makeDecisions bridge ocean delta lemming =
  let distance = stats lemming |> attribute "distance" |> Maybe.map distanceValue |> Maybe.withDefault defaultDistance
      bmax = Bridge.terminus bridge
      (dzx1, dzx2) = Ocean.waveBridgeDangerZone ocean
      inDangerZone = Misc.within dzx1 dzx2 (xPos lemming)
      shouldStopRunning = isFleeingStorm lemming && Misc.within (bmax - 1.5 - distance) bmax (xPos lemming)
      canContinueCrossing = xPos lemming <= (bmax - 1.5 - distance)
      willSneak = False
      directionalMode = if Bridge.complete bridge && (xPos lemming) > 300 then 1 else -1
      timeToThinkAboutFleeingWave = (Ocean.oceanWave ocean |> Ocean.waveValue) <= 10
  in
  if Ocean.isWaveCrashing ocean && inDangerZone then -- too late to flee now...
    lemming |> tossLemming
  else if xPos lemming > 620 then 
    Rescued (xPos lemming, yPos lemming)
  else if timeToThinkAboutFleeingWave && inDangerZone then
    updateImpulse (always (FleeWave directionalMode)) lemming
  else if not canContinueCrossing then
    updateImpulse (always Pray) lemming
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
    Falling _ _ _ -> updateFalling delta lemming
    Sneaking _ _ _ -> run bridge ocean delta lemming
    _ -> lemming
  

update bridge ocean delta =
  List.map (updateLemming bridge ocean delta)
  >> List.filter isUpdatable
