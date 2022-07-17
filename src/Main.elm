port module Main exposing (main)

import Audio
import Basics.Extra exposing (flip, uncurry)
import Browser
import Browser.Events
import Browser.Dom
import Config
import Game
import GFXAsset
import Html
import Html.Attributes as Hats
import Html.Events as Emits
import Json.Decode
import Json.Encode
import Key
import KeyNames exposing (keyNames)
import Task
import Time

port audioPortToJS : Json.Encode.Value -> Cmd msg
port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

main : Platform.Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main = 
  Audio.elementWithAudio
  --Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , audio = audio
    , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
    }

type Model
  = Title Config.Config Audio Video Clock
  | Menu Config.Config Audio Video Clock
  | Options Config.Config Audio Video Clock
  | Game Config.Config Audio Video Clock Game.Game
  | Credits Config.Config Audio Video Clock


mapModelConfig fn model =
  case model of 
    Title config a v c -> Title (fn config) a v c
    Menu config a v c -> Menu (fn config) a v c
    Options config a v c -> Options (fn config) a v c
    Game config a v c g -> Game (fn config) a v c g
    Credits config a v c -> Credits (fn config) a v c
mapModelAudio fn model =
  case model of 
    Title config a v c -> Title config (fn a) v c
    Menu config a v c -> Menu config (fn a) v c
    Options config a v c -> Options config (fn a) v c
    Game config a v c g -> Game config (fn a) v c g
    Credits config a v c -> Credits config (fn a) v c
mapModelVideo fn model =
  case model of 
    Title config a v c -> Title config a (fn v) c
    Menu config a v c -> Menu config a (fn v) c
    Options config a v c -> Options config a (fn v) c
    Game config a v c g -> Game config a (fn v) c g
    Credits config a v c -> Credits config a (fn v) c
mapModelClock fn model =
  case model of 
    Title config a v c -> Title config a v (fn c)
    Menu config a v c -> Menu config a v (fn c)
    Options config a v c -> Options config a v (fn c)
    Game config a v c g -> Game config a v (fn c) g
    Credits config a v c -> Credits config a v (fn c)
mapModelGame fn model =
  case model of 
    Game config a v c g -> Game config a v c (fn g)
    _ -> model
    
getModelConfig model =
  case model of 
    Title c _ _ _ -> c
    Menu c _ _ _ -> c
    Options c _ _ _ -> c
    Game c _ _ _ _ -> c
    Credits c _ _ _ -> c
getModelAudio model =
  case model of 
    Title _ a _ _ -> a
    Menu _ a _ _ -> a
    Options _ a _ _ -> a
    Game _ a _ _ _ -> a
    Credits _ a _ _ -> a
getModelVideo model =
  case model of 
    Title _ _ v _ -> v
    Menu _ _ v _ -> v
    Options _ _ v _ -> v
    Game _ _ v _ _ -> v
    Credits _ _ v _ -> v
getModelClock model =
  case model of 
    Title _ _ _ c -> c
    Menu _ _ _ c -> c
    Options _ _ _ c -> c
    Game _ _ _ c _ -> c
    Credits _ _ _ c -> c
getModelGame model =
  case model of 
    Game _ _ _ _ g -> g
    _ -> Game.newGame (getModelConfig model) (Time.millisToPosix 0)

type alias Video =
  { width : Int
  , height : Int
  , delta : Float
  }

type alias Audio =
  { music : Maybe Audio.Source
  , musicStartTime : Time.Posix
  , soundOn : Bool
  }

newAudio = Audio Nothing (Time.millisToPosix 0) True

type Clock = Clock Time.Posix
newClock = Clock <| Time.millisToPosix 0
clockValue (Clock t) = t


videoWidth {width} = width
videoHeight {height} = height
videoFrameDelta {delta} = delta

type Msg
  = UpdateVideoSystem VideoMsg
  | GameMessage Game.Msg
  | Tick Time.Posix
  | SoundLoaded (Result Audio.LoadError Audio.Source)
  | ViewOptions
  | ViewCredits
  | StartGame
  | ViewMainMenu

type VideoMsg 
  = SetWindowSize Int Int
  | SetFrameDelta Float

setWindowSize w h = UpdateVideoSystem <| SetWindowSize w h
setFrameDelta = UpdateVideoSystem << SetFrameDelta


audio : Audio.AudioData -> Model -> Audio.Audio
audio _ model =
  Maybe.map (flip Audio.audio (getModelClock model |> clockValue)) (getModelAudio model |> .music)
  |> Maybe.withDefault Audio.silence

video : Video -> List (Html.Attribute msg)
video {width,height} =
  let scaleX = (toFloat width) / (toFloat GFXAsset.bgWidth)
      scaleY = (toFloat height) / (toFloat GFXAsset.bgHeight)
      scale = min scaleX scaleY |> String.fromFloat
  in
  [ Hats.style "width" (GFXAsset.bgWidth |> String.fromInt |> flip (++) "px") 
  , Hats.style "height" (GFXAsset.bgHeight |> String.fromInt |> flip (++) "px") 
  , Hats.style "transform" ("scale(" ++ scale ++ ")") 
  , Hats.style "transform-origin" "center top"
  ]

newModel =
  --Title Config.Config Audio Video Clock
  Menu Config.newConfig newAudio (Video 0 0 0.0) newClock
  --Options Config.Config Audio Video Clock
  -- Game Config.newConfig newAudio (Video 0 0 0.0) (newClock) (Game.newGame Config.newConfig (Time.millisToPosix 0))
  --Credits Config.Config Audio Video Clock

init : () -> (Model, Cmd Msg, Audio.AudioCmd Msg)
init _ =
    ( newModel
    , Cmd.batch
      [ Browser.Dom.getViewport |> Task.attempt (\r ->
            case r of
                    Ok vp ->
                            setWindowSize (floor vp.viewport.width) (floor vp.viewport.height)
                    Err e ->
                            setWindowSize 0 0
            )
      , Task.perform Tick Time.now --(Game.Tick >> GameMessage) Time.now
      ]
    , Audio.loadAudio SoundLoaded "assets/Respite.mp3"
    )

update : Audio.AudioData -> Msg -> Model -> (Model, Cmd Msg, Audio.AudioCmd Msg)
update _ msg model =
  case msg of
    ViewOptions ->
      ( Options (getModelConfig model) (getModelAudio model) (getModelVideo model) (getModelClock model)
      , Cmd.none
      , Audio.cmdNone
      )
    ViewCredits ->
      ( Credits (getModelConfig model) (getModelAudio model) (getModelVideo model) (getModelClock model)
      , Cmd.none
      , Audio.cmdNone
      )
    ViewMainMenu ->
      ( Menu (getModelConfig model) (getModelAudio model) (getModelVideo model) (getModelClock model)
      , Cmd.none
      , Audio.cmdNone
      )
    StartGame ->
      ( Game (getModelConfig model) (getModelAudio model) (getModelVideo model) (getModelClock model) (Game.newGame (getModelConfig model) (getModelClock model |> clockValue))
      , Cmd.none
      , Audio.cmdNone
      )
    UpdateVideoSystem vmsg -> 
      updateVideo vmsg model
    SoundLoaded (Ok sound) ->
      ( model
        |> mapModelAudio (\a -> {a | music = Just sound})
      , Cmd.none
      , Audio.cmdNone
      )
    SoundLoaded (Err e) ->
      let
          errr = Debug.log "Sound loading error" e
      in
      (model, Cmd.none, Audio.cmdNone)
    Tick t ->
      ( model
        |> mapModelClock (always (Clock t))
        |> mapModelGame (Game.update (Game.Tick t))
      , Cmd.none
      , Audio.cmdNone
      )
    GameMessage (Game.ExitGame) ->
      ( Menu (getModelConfig model) (getModelAudio model) (getModelVideo model) (getModelClock model)
      , Cmd.none
      , Audio.cmdNone
      )
    GameMessage gmsg ->
      ( mapModelGame (Game.update gmsg) model
      , Cmd.none
      , Audio.cmdNone
      )


updateVideo : VideoMsg -> Model -> (Model, Cmd Msg, Audio.AudioCmd Msg)
updateVideo vmsg model =
  case vmsg of
    SetWindowSize width height ->
      ( mapModelVideo (\v -> { v | width = width, height = height }) model
      , Cmd.none
      , Audio.cmdNone
      )
    SetFrameDelta delta ->
      ( mapModelVideo (\v -> { v | delta = delta }) model
      , Cmd.none
      , Audio.cmdNone
      )

subscriptions : Audio.AudioData -> Model -> Sub Msg
subscriptions _ _ = 
  Sub.batch
    [ {-Browser.Events.onAnimationFrameDelta setFrameDelta
    , -}Browser.Events.onResize setWindowSize
    , Time.every 100 (Game.Tick >> GameMessage)
    , Browser.Events.onKeyDown (Json.Decode.map (Game.KeyDown >> GameMessage) Key.decoder)
    , Browser.Events.onKeyUp (Json.Decode.map (Game.KeyUp >> GameMessage) Key.decoder)
    ]

view : Audio.AudioData -> Model -> Html.Html Msg
view _ model =
  let viewScreen = model |> getModelVideo |> video |> Html.main_
  in
  case model of
    (Game _ _ _ _ _) as game -> 
      model
      |> getModelGame
      |> Game.view
      |> viewScreen
      |> Html.map GameMessage
    (Title _ _ _ _) ->
      viewScreen
        [ Html.h1 [] [ Html.text "Salvatore's Bridge" ] ]
    (Credits _ _ _ _) ->
      viewScreen
        [ Html.h1 [] [ Html.text "Credits" ]
        , Html.h2 [] [ Html.text "Programming, Writing, Graphics" ]
        , Html.h3 [] [ Html.text "Zachariah" ]
        , Html.button [ Emits.onClick ViewMainMenu ] [ Html.text "Go Back" ]
        ]
    (Menu _ _ _ _) ->
      viewScreen
        [ Html.h1 [] [ Html.text "Salvatore's Bridge" ]
        , mainMenu 
        ]
    (Options conf _ _ _) ->
      viewScreen
        [ Html.h1 [] [ Html.text "Options" ]
        , Html.button [ Emits.onClick ViewMainMenu ] [ Html.text "Go Back" ]
        ]


mainMenu = 
  [ ("New Game", StartGame)
  , ("Options", ViewOptions)
  , ("Credits", ViewCredits)
  ] 
  |> List.map (uncurry menuButton)
  |> Html.ul []


menuButton string msg =
  Html.li []
    [ Html.button 
      [ Emits.onClick msg ]
      [ Html.text string ]
    ]
