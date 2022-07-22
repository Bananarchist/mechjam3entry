module GFXAsset exposing (bg, viewSprite, mechStandSprite, mechPickUpSprite, mechHoldingSprite, segmentSprite, frontTide, backTide, waveSprite, lemmingSprite, fadeInText, withSendMessageBtn, bgWidth, bgHeight, distantBg, crashingWaveSprite, rain, warning)

import Basics.Extra exposing (flip, uncurry)
import Dict
import Dict.Extra
import Html
import Html.Attributes as Hats
import Html.Events as Emits

intToPixels =
  String.fromInt >> flip (++) "px"

floatToPixels =
  String.fromFloat >> flip (++) "px"

type alias Coords = (Int, Int)
coordX = Tuple.first
coordY = Tuple.second
type alias Dimensions = (Int, Int)
width = Tuple.first
height = Tuple.second
type alias Nudge = (Int, Int)
nudgeX = Tuple.first
nudgeY = Tuple.second
type alias Rotate = Float
type alias Degrees = Float
type Flip 
  = Horizontally
  | Vertically
  | Both
  | Neither


type SpriteBox
  = Box Coords Dimensions
  | Adjusted SpriteBox Nudge Flip Rotate

type Transformation
  = Translated (Float, Float)
  | Mirrored Flip
  | Scaled (Float, Float)
  | Rotated Degrees

transformationCss : Transformation -> List (String, String)
transformationCss tx =
  let pixelPair x y = (String.fromFloat x) ++ "px, " ++ (String.fromFloat y) ++ "px"
  in
  case tx of
    Translated (x, y) -> [("transform", "translate(" ++ pixelPair x y ++ ")")]
    Mirrored Neither -> []
    Mirrored _ -> [("transform", "scaleX(-1)")]
    Scaled (x,y) -> [("transform", "scale(" ++ (String.fromFloat x) ++ ", " ++ (String.fromFloat y) ++ ")")]
    Rotated a -> [("transform", "rotate(" ++ (degrees a |> String.fromFloat) ++ "rad)")]

transformationsAsStyleAttributes : List Transformation -> List (Html.Attribute msg)
transformationsAsStyleAttributes =
  List.concatMap transformationCss
  >> Dict.Extra.fromListDedupe (\a b -> a ++ " " ++ b) 
  >> Dict.foldr (\k v a -> (Hats.style k v) :: a) []

coords sb =
  case sb of 
    Box c _ -> c
    Adjusted nsb _ _ _ -> coords nsb

dims sb =
  case sb of
    Box _ d -> d
    Adjusted nsb _ _ _ -> dims nsb

nudge sb =
  case sb of
    Adjusted _ n _ _ -> n
    _ -> (0,0)

flipped sb =
  case sb of
    Adjusted _ _ f _ -> f
    _ -> Neither

flipX f =
  case f of
    Horizontally -> True
    Both -> True
    _ -> False

flipY f =
  case f of
    Vertically -> True
    Both -> True
    _ -> False

rotation sb =
  case sb of
    Adjusted _ _ _ r -> r
    _ -> 0

viewSpriteWithAttrs attrs sb txs =
    [ Hats.class "sprite-sheet"
    , Hats.style "width" (sb |> dims |> width |> String.fromInt |> flip (++) "px")
    , Hats.style "height" (sb |> dims |> height |> String.fromInt |> flip (++) "px")
    , Hats.style "background-position"
      (String.concat 
        [ "-"
        , (sb |> coords |> coordX |> String.fromInt |> flip (++) "px")
        , " -"
        , (sb |> coords |> coordY |> String.fromInt |> flip (++) "px")
        ]
      )
    ] 
    ++ (transformationsAsStyleAttributes txs)
    ++ attrs
    |> flip Html.div []

viewSprite : SpriteBox -> List Transformation -> Html.Html msg
viewSprite =
  viewSpriteWithAttrs []


fadeInText : List String -> List (Html.Html msg)
fadeInText = 
  List.map (Html.text >> List.singleton >> Html.p [])

withSendMessageBtn : msg -> String -> List (Html.Html msg) -> List (Html.Html msg)
withSendMessageBtn msg label list =
  Html.text label
  |> List.singleton
  |> Html.button [ Emits.onClick msg ]
  |> List.singleton
  |> List.append list

bgWidth = 750
bgHeight = 400
bg = viewSprite (Box (410, 160) (bgWidth, bgHeight)) []
distantBg = viewSpriteWithAttrs [ Hats.style "opacity" "0.3" ] (Box (1078, 663) (bgWidth, bgHeight)) [Translated (0, 40)]


mechArmRightStillBox = Box (806, 925) (66, 80) 
mechArmLeftStillBox = Box (727, 926) (63, 78) 
mechArmRightExtendedBox = Box (494, 916) (70, 96) 
mechArmLeftExtendedBox = Box (418, 920) (71, 89) 
mechTorsoStraight = Box (1102, 924) (43, 114) 
mechLegLeftStraight = Box (893, 920) (49, 82) 
mechLegRightStraight = Box (957, 920) (47, 81) 
mechLegLeftBrace = Box (581, 922) (54, 78) 
mechLegRightBrace = Box (654, 925) (58, 76) 


mechStandBox = Box (586, 743) (85, 178)
mechPickUpBox = Box (684, 743) (113, 178)
mechHoldingBox = Box (805, 743) (77, 178)

mechStandSprite leftFacing xDistance = 
    viewSprite mechStandBox [Translated (xDistance, 200), Mirrored (if leftFacing then Horizontally else Neither)]
    {-[ viewSprite mechArmLeftStillBox [Translated (xDistance+2, 225), Mirrored (if leftFacing then Horizontally else Neither ), Rotated 90 ]
    , viewSprite mechLegLeftStraight [Translated (xDistance+7, 286), Mirrored (if leftFacing then Horizontally else Neither )]
    , viewSprite mechTorsoStraight [Translated (xDistance, 200), Mirrored (if leftFacing then Horizontally else Neither )]
    , viewSprite mechLegRightStraight [Translated (xDistance+7, 286), Mirrored (if leftFacing then Horizontally else Neither )]
    , viewSprite mechArmRightStillBox [Translated (xDistance+2, 225), Mirrored (if leftFacing then Horizontally else Neither ), Rotated 90 ]
    ]-}

mechPickUpSprite leftFacing xDistance =
    viewSprite mechPickUpBox [Translated (xDistance, 200), Mirrored (if leftFacing then Horizontally else Neither)]

mechHoldingSprite leftFacing xDistance =
    viewSprite mechHoldingBox [Translated (xDistance, 200), Mirrored (if leftFacing then Horizontally else Neither)]

segmentBox = Box (422, 843) (119, 17)

segmentSprite x y rot leftFacing highlighted =
  viewSpriteWithAttrs [ Hats.classList [( "highlighted", highlighted )] ]
    segmentBox [Translated (x, y), Mirrored (if leftFacing then Horizontally else Neither), Rotated rot]
  --viewSprite (Adjusted segmentBox (x, y) (if leftFacing then Horizontally else Neither) rot) []

lemmingBox = Box (465, 770) (7,21)

lemmingSprite x y s = 
  viewSprite lemmingBox [Translated (x, (y + (21 - 21 * s))), Scaled (1, s) ]

waveBox = Box (1271, 161) (638, 123)

waveCrashingBox = Box (0,0) (1,1)

waveSprite magnification crashing =
  if crashing then
    viewSprite waveCrashingBox []
  else
    viewSprite waveBox [Translated (0,200), Scaled (magnification, magnification)]

crashingWaveSprite value =
  let scale = (abs value + 1)
  in
  [ viewSprite waveBox [Translated (0, 200 + (abs value * 200)), Scaled (scale, scale)]]

backTide level =
  let fill = level / 10
      h = 150 + fill
  in
  Html.div 
      (transformationsAsStyleAttributes [Translated (0, (400 - h))]
        |> (++)
          [ Hats.id "back-tide"
          , Hats.style "width" "750px"
          , Hats.style "height" (h |> floatToPixels)
          ]
        )
      []

frontTide level =
  Html.div 
      (transformationsAsStyleAttributes [Translated (0, (400 - level))]
        |> (++)
          [ Hats.id "front-tide"
          , Hats.style "width" "750px"
          , Hats.style "height" (level |> floatToPixels)
          ]
        )
      []

rain =
  Html.div [ Hats.id "rain" ] []

warning =
  Html.div [ Hats.id "warning" ] []
