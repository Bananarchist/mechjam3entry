module Bridge exposing (Bridge, newBridge, Segment(..), view, xPos, yPos, angle, length, segmentList, liftBridgeSegment, update, dropBridgeSegment, placeBridgeSegment, placeOpen, placeable, complete, terminus, anyHeld, viewPlaced)

import Basics.Extra exposing (flip, uncurry)
import GFXAsset
import List.Extra
import Maybe.Extra
import Misc


type alias Bridge =
  { seg1 : Segment
  , seg2 : Segment
  , seg3 : Segment
  }

type alias RemainingMillis = Int
type alias LeftFacing = Bool

type Segment 
  = Placed Int
  | Held Float LeftFacing
  | Liftable (Float, Float) Float
  | PickingUp (Float, Float) LeftFacing RemainingMillis
  | Stray (Float, Float) Float
  -- Falling Float RemainingMillis


newBridge = Bridge (Stray (0,320) (degrees -120)) (Stray (560,300) (degrees -45)) (Stray (20,280) (degrees -160))

length = 119
bridgeStart = 171

segmentList : Bridge -> List Segment
segmentList {seg1, seg2, seg3} =
  [seg1,seg2,seg3]

bridgeFromList : List Segment -> Maybe Bridge
bridgeFromList segments =
  case segments of
    (s1 :: s2 :: s3 :: _) -> Just (Bridge s1 s2 s3)
    _ -> Nothing

isLiftable segment =
  case segment of
    Liftable _ _ -> True
    _ -> False

isHeld segment =
  case segment of 
    Held _ _ -> True
    _ -> False

isStray segment =
  case segment of
    Stray _ _ -> True
    _ -> False

isPlaced segment =
  case segment of
    Placed _ -> True
    _ -> False

isPickingUp segment =
  case segment of
    PickingUp _ _ _ -> True
    _ -> False

setToStray segment =
  Stray (coords segment) (angle segment)

setToLiftable segment =
  Liftable (coords segment) (angle segment)

finishPickingUp segment =
  case segment of 
    PickingUp (x, y) lf _ -> Held x lf
    _ -> segment

placement segment =
  case segment of
    Placed x -> Just x
    _ -> Nothing

terminus : Bridge -> Float
terminus bridge =
  segmentList bridge
  |> List.indexedMap (\i _ -> i)
  |> List.Extra.takeWhile (not << placeOpen bridge)
  |> List.length
  |> (*) length
  |> (+) bridgeStart
  |> toFloat

placeOpen bridge idx =
  segmentList bridge
  |> List.map placement
  |> Maybe.Extra.values
  |> List.all ((/=) idx)

anyHeld = segmentList >> List.any isHeld
anyLiftable = segmentList >> List.any isLiftable

openPlaces bridge =
  segmentList
  >> List.filter (not << isPlaced)
  >> List.indexedMap Tuple.pair
  >> List.map Tuple.first

complete =
  segmentList 
  >> List.all isPlaced

drop segment =
  case segment of
    Held x _ -> 
      if x <= 30 then
        Stray (x, 280 + x * 1.5) (degrees <| x * -7.5)
      else if x >= 550 then
        Stray (x, x - 240) (degrees <| (x - 540) * -4)
      else if 200 <= x && x <= 500 then
        Stray (x, max 350 (280 + (x - 200))) (degrees 0)
      else 
        Stray (x, 300) (degrees 0)
    _ -> segment

placeable bridge segment =
  case segment of
    Held unadjustedX _ -> 
      let x = unadjustedX --+ (length / 2)
      in
      bridgeIndexForX x
      |> Maybe.map (placeOpen bridge)
      |> Maybe.withDefault False
    _ -> False

bridgeCoordsForIndex idx =
  (bridgeStart + idx * length, 180)

bridgeIndexForX x =
  if Misc.within bridgeStart (bridgeStart + length) x then
    Just 0
  else if Misc.within (bridgeStart + length) (bridgeStart + length * 2) x then
    Just 1
  else if Misc.within (bridgeStart + length * 2) (bridgeStart + length * 3) x then 
    Just 2
  else
    Nothing

place segment =
  case segment of
    Held x _ -> 
      bridgeIndexForX x
      |> Maybe.map Placed
      |> Maybe.withDefault segment
    _ -> segment

updateSegments mapper ({seg1,seg2,seg3} as bridge) =
  { bridge 
  | seg1 = mapper seg1
  , seg2 = mapper seg2
  , seg3 = mapper seg3
  }



updateSegmentAtIndex updater index bridge =
  case index of
    0 -> { bridge | seg1 = updater bridge.seg1 }
    1 -> { bridge | seg2 = updater bridge.seg2 }
    2 -> { bridge | seg3 = updater bridge.seg3 }
    _ -> bridge

nearEnoughToPickup mechX segment =
  let 
      xs = xPos segment
  in
  Misc.within xs (xs + length) mechX

updatePredicatedSegment : (Segment -> Bool) -> (Segment -> Segment) -> Bridge -> Maybe Bridge
updatePredicatedSegment predicate updater bridge =
  bridge
  |> segmentList
  |> List.indexedMap Tuple.pair
  |> List.partition (Tuple.second >> predicate)
  |> Tuple.first
  |> List.head
  |> Maybe.map (Tuple.first >> (flip (updateSegmentAtIndex updater) bridge))

liftBridgeSegment : Float -> (Bridge -> a) -> Bridge -> Maybe a
liftBridgeSegment location updater bridge =
  updatePredicatedSegment (\s -> nearEnoughToPickup location s && isLiftable s) (coords >> (flip (flip PickingUp False) 350)) bridge
  |> Maybe.map updater

dropBridgeSegment : Bridge -> Bridge
dropBridgeSegment bridge =
    updatePredicatedSegment isHeld drop bridge
    |> Maybe.withDefault bridge

placeBridgeSegment : Bridge -> Bridge 
placeBridgeSegment bridge =
  updatePredicatedSegment (placeable bridge) place bridge
  |> Maybe.withDefault bridge

coords =
  Misc.duple
  >> Tuple.mapBoth xPos yPos

xPos segment = 
  case segment of
    Placed x -> bridgeStart + (x |> toFloat) * length
    Held x _ -> x
    Stray (x,_) _ -> x
    Liftable (x,_) _ -> x
    PickingUp (x, _) lf rm -> x

yPos segment =
  case segment of
    Placed _ -> 164
    Held _ _ -> 180
    PickingUp (_,y) _ rm -> 
      if Misc.within 350 150 rm then
        y
      else 
        280
    Stray (_,y) _ -> y
    Liftable (_,y) _ -> y

angle segment =
  case segment of
    Stray _ a -> a
    Liftable _ a -> a
    _ -> 0

leftFacing segment =
  case segment of
    Held _ True -> True
    _ -> False

remainingMillis segment =
  case segment of
    PickingUp _ _ rm -> rm
    _ -> 0

setXPos : Float -> Segment -> Segment
setXPos x segment =
  case segment of 
    Held _ f -> Held x f
    Stray (_, y) a -> Stray (x,y) a
    Liftable (_, y) a -> Liftable (x,y) a
    _ -> segment

mapRemainingMillis : (Int -> Int) -> Segment -> Segment
mapRemainingMillis fn segment =
  case segment of
    PickingUp cooo lf rm -> PickingUp cooo lf (fn rm)
    _ -> segment
  
updateRemainingMillis : Float -> Bridge -> Bridge
updateRemainingMillis delta bridge =
  let hasJustFinishedLifting = (Misc.duple >> Tuple.mapBoth isPickingUp (remainingMillis >> ((>=)0)) >> uncurry (&&))
  in
  segmentList bridge
  |> List.map (mapRemainingMillis (flip (-) (floor delta)))
  |> bridgeFromList
  |> Maybe.andThen (updatePredicatedSegment hasJustFinishedLifting finishPickingUp)
  |> Maybe.withDefault bridge

mapSegmentIf : (Segment -> Bool) -> (Segment -> Segment) -> Segment -> Segment
mapSegmentIf predicate mapper segment =
  if predicate segment then
    mapper segment
  else
    segment


updateSegment mechXPos delta bridge =
  let noLongerAbleToBeLifted = (\s -> isLiftable s && not (nearEnoughToPickup mechXPos s))
      newlyAbleToBeLifted = (\s -> not (anyHeld bridge) && isStray s && nearEnoughToPickup mechXPos s)
      finishedBeingPickedUp = (\s -> isPickingUp s && (remainingMillis s) <= 0)
  in
  mapRemainingMillis (flip (-) (floor delta))
  >> mapSegmentIf noLongerAbleToBeLifted setToStray
  >> mapSegmentIf newlyAbleToBeLifted setToLiftable
  >> mapSegmentIf finishedBeingPickedUp finishPickingUp
  >> mapSegmentIf isHeld (setXPos mechXPos)


update : Float -> Float -> Bridge -> Bridge
update mechXPos delta bridge =
  updateSegments (updateSegment mechXPos delta bridge) bridge
  {-  if anyHeld bridge then 
    updatePredicatedSegment isHeld (setXPos mechXPos) bridge
      |> Maybe.withDefault bridge
  else if List.any  then
    bridge 
    |> updatePredicatedSegment (\s -> nearEnoughToPickup mechXPos s && not isPlaced s) (setLiftable)

    bridge
    |> updateRemainingMillis delta 
  -}
  

viewSegment segment = 
  GFXAsset.segmentSprite (xPos segment) (yPos segment) (angle segment) (leftFacing segment) (isLiftable segment)

view =
  segmentList
  >> List.filter (not << isPlaced)
  >> List.map viewSegment

viewPlaced =
  segmentList
  >> List.filter isPlaced
  >> List.map viewSegment
