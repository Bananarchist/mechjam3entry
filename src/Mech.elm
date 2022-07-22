module Mech exposing (Model, newMech, xPos, isFacingLeft, velocity, view, setX, setLeftFacing, setVelocity, update, startLifting, hasDrowned, hasBeenStruck)

import Bridge
import Basics.Extra exposing (flip, uncurry)
import GFXAsset
import Ocean
import Misc

type alias Velocity = Int
type alias Position = Float
type alias LeftFacing = Bool
type alias RemainingMillis = Int

type Model 
  = Mech LeftFacing Position Velocity
  | Carrying LeftFacing Position Velocity
  | Lifting LeftFacing Position RemainingMillis
  | Struck LeftFacing Position RemainingMillis
  | Drowned LeftFacing Position RemainingMillis


newMech = Mech False 0.0 0
startLifting mech = Lifting (mech |> isFacingLeft) (mech |> xPos) 350
isLifting mech =
  case mech of
    Lifting _ _ _ -> True
    _ -> False
isCarrying mech =
  case mech of
    Carrying _ _ _ -> True
    _ -> False
hasDrowned mech = 
  case mech of
    Drowned _ _ _ -> True
    _ -> False
hasBeenStruck mech =
  case mech of 
    Struck _ _ _ -> True
    _ -> False
completedCarrying mech =
  Mech (isFacingLeft mech) (xPos mech) 0
mechCompletedLifting mech =
  Carrying (isFacingLeft mech) (xPos mech) 0
struckByWave mech =
  Struck (isFacingLeft mech) (xPos mech) 350
drownInTide mech =
  Drowned (isFacingLeft mech) (xPos mech) 150

xPos mech = 
  case mech of 
    Mech _ x _ -> x
    Carrying _ x _ -> x
    Lifting _ x _ -> x
    Struck _ x _ -> x
    Drowned _ x _ -> x
isFacingLeft mech =
  case mech of
    Mech f _ _ -> f
    Carrying f _ _ -> f
    Lifting f _ _ -> f
    Struck f _ _ -> f
    Drowned f _ _ -> f
velocity mech =
  case mech of
    Mech _ _ v -> v
    Carrying _ _ v -> v
    _ -> 0
remainingMillis mech =
  case mech of 
    Lifting _ _ m -> m
    Struck _ _ m -> m
    Drowned _ _ m -> m
    _ -> 0
setX x mech =
  case mech of 
    Mech f _ v -> Mech f x v
    Carrying f _ v -> Carrying f x v
    Lifting f _ m -> Lifting f x m
    Struck f _ m -> Struck f x m
    Drowned f _ m -> Drowned f x m
setLeftFacing f mech =
  case mech of 
    Mech _ x v -> Mech f x v
    Carrying _ x v -> Carrying f x v
    Lifting _ x m -> Lifting f x m
    _ -> mech
    -- commented because public interface was getting risky
    -- Struck _ x m -> Struck f x m
    -- Drowned _ x m -> Drowned f x m
setVelocity v mech =
  case mech of 
    Mech f x _ -> Mech f x v
    Carrying f x _ -> Carrying f x v
    _ -> mech
setRemainingMillis m mech =
  case mech of 
    Lifting f x _ -> Lifting f x m
    Struck f x _ -> Struck f x m
    Drowned f x _ -> Drowned f x m
    _ -> mech

velocitySign lf = if lf then -1 else 1


-- TODO: the order of updating etc is a little out of wack here
checkSegmentInteractions bridge delta mech =
  let rm = (remainingMillis mech) - (floor delta)
  in
  if isLifting mech then
    if rm <= 0 then
      mechCompletedLifting mech
    else 
      setRemainingMillis rm mech
  else if isCarrying mech && not (Bridge.anyHeld bridge) then
    completedCarrying mech
  else 
    mech


checkOceanInteractions ocean delta mech =
  let inWaveDangerZone = (uncurry Misc.within (Ocean.waveSurfaceDangerZone ocean)) (xPos mech)
      inTidalDangerZone = (uncurry Misc.within (Ocean.tidalDangerZone ocean)) (xPos mech)
  in
  if Ocean.isWaveCrashing ocean && inWaveDangerZone then
    struckByWave mech
  else if inTidalDangerZone then
    drownInTide mech
  else
    mech

update : { state | mech : Model, bridge : Bridge.Bridge, ocean : Ocean.Ocean} -> Float -> Model
update {mech, bridge, ocean} delta =
  let v = velocity mech
  in
  if v > 0 then
    (toFloat v) / 15.0 * delta 
    |> (*) (mech |> isFacingLeft |> velocitySign)
    |> (+) (mech |> xPos)
    |> clamp 0 610
    |> flip setX mech
  else
    let rm = (remainingMillis mech) - (floor delta)
    in
    mech
    |> checkSegmentInteractions bridge delta
    |> checkOceanInteractions ocean delta 

    {-
type MechElement 
  = ArmRight
  | ArmLeft
  | Torso
  | LegRight
  | LegLeft

type Side = Left | Right
type Arm = Arm Side
type Leg = Leg Side
type Body = Body

type MechPose ar al t lr ll = MechPose ar al t lr ll


type Uneq = Uneq

initMechPose : MechPose Uneq Uneq Uneq Uneq Uneq
initMechpose = MechPose Uneq Uneq Uneq Uneq Uneq

viewMechPose : MechPose (Arm Right) (Arm Left) Body (Leg Right) (Leg Left) -> [Html.Html msg]
viewMechPose MechPose al ar b ll lr =
  [ drawArmLeft
  , drawLegLeft
  , drawTorso
  , drawLegRight
  , drawArmRight
  ]
    -}



view mech = 
  if isLifting mech then 
    if mech |> remainingMillis |> Misc.within 0 200 then
      [ GFXAsset.mechPickUpSprite (mech |> isFacingLeft) (mech |> xPos) ]
    else
      [ GFXAsset.mechStandSprite (mech |> isFacingLeft) (mech |> xPos)]
  else if isCarrying mech then
    [ GFXAsset.mechHoldingSprite (mech |> isFacingLeft) (mech |> xPos) ]
  else
    [GFXAsset.mechStandSprite (mech |> isFacingLeft) (mech |> xPos)]
