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
  | SomethingElse

type Lemming
  = Living LemmingStats Position Impulse
    --{ position : Position }
  | Rescued Position
  | Falling Position RemainingTime
  | Dead

defaultSpeed = 50.0
newLemming statistics = Living statistics (0, 145) FleeStorm
newStats = Dict.fromList [("speed", Speed defaultSpeed)]
newPopulation = List.repeat 99 (newLemming newStats)

{- We want to check if hasSpeedState - if not, create one with rand val
if so, then (later) we should look to other rand vals to introduce such
as mapping to risk taker (eg, tightrope walker) -}

mapRandomValues : List Lemming -> List Float -> (List Float, List Lemming)
mapRandomValues lemmings lval =
  List.take (List.length lemmings) lval
  |> List.Extra.zip lemmings
  |> Debug.log "Zipped"
  |> List.map (\(l, s) -> mapAttrs (mapSpeedStat (always s)) l)
  |> Tuple.pair (List.drop (List.length lemmings) lval)

{-
hasSpeedStat : Lemming -> Bool
hasSpeedStat =
  stats 
  >> Maybe.map (List.any isSpeedAttr) 
  >> Maybe.withDefault False
-}

mapSpeedStat : (Float -> Float) -> LemmingStats -> LemmingStats
mapSpeedStat updater =
  Dict.update "speed" (Maybe.map (speedValue >> updater >> Speed))

mapAttrs : (LemmingStats -> LemmingStats) -> Lemming -> Lemming
mapAttrs updater lemming =
  case lemming of
    Living attrs p i -> Living (updater attrs) p i
    _ -> lemming

isSpeedAttr attr =
  case attr of
    Speed _ -> True
    _ -> False

speedValue attr =
  case attr of
    Speed v -> v
    _ -> defaultSpeed

tossLemming lemming = 
  case lemming of 
    Living _ (x, y) _ -> Falling (x, y) 300
    _ -> lemming

xPos lemming =
  case lemming of
    Living _ (x, _) _ -> x
    Falling (x, _) _ -> x
    Dead -> 0
    Rescued (x, _) -> x

yPos lemming =
  case lemming of
    Living _ (_, y) _ -> y
    Falling (_, y) _ -> y
    Dead -> 0
    Rescued (_, y) -> y

stats lemming =
  case lemming of
    Living attrs _ _ -> attrs
    _ -> Dict.empty 

attribute =
  Dict.get

isAlive lemming =
  case lemming of
    Living _ _ _ -> True
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
        FleeStorm -> 2
        FleeWave -> if xPos lemming > 300 then 2 else -2
        _ -> 0
    )
  |> flip Maybe.Extra.andMap (Maybe.map (*) speedAttr)
  |> Maybe.withDefault 0

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
  updateX ((+) ((runSpeed lemming) * delta/100)) lemming
  |> makeDecisions bridge ocean delta


viewLemming =
  Misc.duple
  >> Tuple.mapBoth xPos yPos
  >> uncurry GFXAsset.lemmingSprite

view =
  List.map viewLemming

updateLemming bridge ocean delta lemming =
  case lemming of
    Living _ (x, y) i -> run bridge ocean delta lemming
    Falling _ _ -> updateFalling delta lemming
    _ -> lemming
  

update bridge ocean delta =
  List.map (updateLemming bridge ocean delta)
  >> List.filter isAlive
