port module Main exposing (main)

import Audio
import Basics.Extra exposing (flip, uncurry, curry)
import Browser
import Browser.Events
import Browser.Dom
import Config
import Duration
import Game
import GFXAsset
import Html
import Html.Attributes as Hats
import Html.Events as Emits
import Json.Decode
import Json.Encode
import Key
import KeyNames exposing (keyNames)
import Lemmings
import Message
import Random
import Sound
import Task
import Time

port audioPortToJS : Json.Encode.Value -> Cmd msg
port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg

main : Platform.Program () (Audio.Model Msg App) (Audio.Msg Msg)
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

type App
  = Loading Model (Maybe Time.Posix) (Maybe (List Float))
  | Title Model
  | Menu Model
  | Options Model
  | OptionsUpdatingControl Model Config.Control
  | Game Model Game.Game
  | Credits Model

type alias Model =
  { config : Config.Config
  , audio : Audio
  , video : Video
  , clock : Clock
  , random : List Float
  }

newModel =
  { config = Config.newConfig
  , audio = newAudio
  , video = Video 0 0 0.0
  , clock = newClock
  , random = []
  }

isLoading app =
  case app of
    Loading _ _ _ -> True
    _ -> False
  --Title Config.Config Audio Video Clock
  --Menu Config.newConfig newAudio (Video 0 0 0.0) newClock
  --Options Config.Config Audio Video Clock
  -- Game Config.newConfig newAudio (Video 0 0 0.0) (newClock) (Game.newGame Config.newConfig (Time.millisToPosix 0))
  --Credits Config.Config Audio Video Clock
appModel app =
  case app of
    Loading c _ _ -> c
    Title m -> m
    Menu m -> m
    Options m -> m
    OptionsUpdatingControl m _ -> m
    Game m _ -> m
    Credits m -> m

mapModel mapper app =
  case app of
    Loading c t l -> Loading c t l
    Title m -> m |> mapper |> Title
    Menu m -> m |> mapper |> Menu
    Options m -> m |> mapper |> Options
    OptionsUpdatingControl m c -> m |> mapper |> flip OptionsUpdatingControl c
    Game m g -> m |> mapper |> flip Game g
    Credits m -> m |> mapper |> Credits

mapAppConfig fn =
  mapModel (\m -> { m | config = fn m.config })
mapAppAudio fn =
  mapModel (\m -> { m | audio = fn m.audio })
mapAppVideo fn =
  mapModel (\m -> { m | video = fn m.video })
mapAppClock fn =
  mapModel (\m -> { m | clock = fn m.clock })
mapAppRandom fn =
  mapModel (\m -> { m | random = fn m.random })
mapAppGame fn app =
  case app of 
    Game m g -> Game m (fn g)
    _ -> app
    
getAppConfig =
  appModel >> .config
getAppAudio =
  appModel >> .audio
getAppVideo =
  appModel >> .video
getAppClock =
  appModel >> .clock
getAppRandom =
  appModel >> .random
getAppGame app =
  case app of 
    Game _ g -> Just g
    _ -> Nothing
getAppControlToUpdate model =
  case model of
    OptionsUpdatingControl _ control -> Just control
    _ -> Nothing

type alias Video =
  { width : Int
  , height : Int
  , delta : Float
  }

type alias Audio =
  { music : Maybe Audio.Source
  , musicStartTime : Time.Posix
  , soundOn : Bool
  , dropSfx : Maybe Audio.Source
  , sfx :  List (Sound.Sound, Time.Posix)
  }

newAudio = Audio Nothing (Time.millisToPosix 0) True Nothing []

type Clock = Clock Time.Posix
newClock = Clock <| Time.millisToPosix 0
clockValue (Clock t) = t


videoWidth {width} = width
videoHeight {height} = height
videoFrameDelta {delta} = delta

type Msg
  = UpdateVideoSystem VideoMsg
  | GameMessage Game.Msg
  | InitialValues Time.Posix (List Float)
  | Tick Time.Posix
  | SoundLoaded (Result Audio.LoadError Audio.Source)
  | ViewOptions
  | ViewCredits
  | ViewMainMenu
  | UpdateConfig ConfigMsg
  | KeyDown Key.Key
  | KeyUp Key.Key
  | StartGame

type ConfigMsg
  = ListenForKey Config.Control
  | SetMusicVolume Float
  | SetSFXVolume Float

type VideoMsg 
  = SetWindowSize Int Int
  | SetFrameDelta Float

setWindowSize w h = UpdateVideoSystem <| SetWindowSize w h
setFrameDelta = UpdateVideoSystem << SetFrameDelta


audio : Audio.AudioData -> App -> Audio.Audio
audio _ app =
  case app of 
    Game state g ->
      flip List.map (Game.audioSignals g) 
        (\s ->
          case s of
            Game.StartMusic -> 
              Maybe.map (flip Audio.audio (Time.millisToPosix 0)) (getAppAudio app |> .music)
              |> Maybe.map (Audio.scaleVolume (getAppConfig app |> Config.musicVolume))
              |> Maybe.withDefault Audio.silence
            _ -> Audio.silence
        )
        |> Audio.group
    _ ->
      flip List.map (getAppAudio app |> .sfx) (\(sfx, _) ->
        case sfx of
          Sound.Drop -> 
              Maybe.map (flip Audio.audio (getAppClock app |> clockValue)) (getAppAudio app |> .dropSfx)
              |> Maybe.map (Audio.scaleVolume (getAppConfig app |> Config.sfxVolume))
              |> Maybe.withDefault Audio.silence
          _ -> Audio.silence
      )
      |> Audio.group

    

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

newApp model time lemms =
  --Title Config.Config Audio Video Clock
  --Menu newModel |> mapAppSt
  --Options Config.Config Audio Video Clock
  Game model (Game.newGameWithLemmings (Lemmings.newPopulationWithRandomValues lemms) time Config.newConfig)
  --Credits Config.Config Audio Video Clock

generateLemmingValues = 
  [ (GameMessage << Game.GotLemmingValue, Random.list 300 (Random.float 0 100))
  ]

init : () -> (App, Cmd Msg, Audio.AudioCmd Msg)
init _ =
    ( Loading newModel Nothing Nothing
    , [ Browser.Dom.getViewport |> Task.attempt (\r ->
            case r of
                    Ok vp ->
                            setWindowSize (floor vp.viewport.width) (floor vp.viewport.height)
                    Err e ->
                            setWindowSize 0 0
            )
      , Task.perform Tick Time.now --(Game.Tick >> GameMessage) Time.now
      , Cmd.batch (List.map (uncurry Random.generate) generateLemmingValues)
      ]
      |> Cmd.batch
    , Audio.cmdBatch 
      [ Audio.loadAudio SoundLoaded "assets/bgm.mp3"
      , Audio.loadAudio SoundLoaded "assets/drop.mp3"
      ]
    )



update : Audio.AudioData -> Msg -> App -> (App, Cmd Msg, Audio.AudioCmd Msg)
update _ msg app =
  let dropSFX = mapAppAudio (\a -> { a | sfx = (Sound.Drop, (getAppClock app |> clockValue)) :: a.sfx})
      withNone g = (g, Cmd.none, Audio.cmdNone)
      withCmds (g, cmds) c = (g, Cmd.batch (c ++ cmds))
      withAudioNone (g, c) = (g, c, Audio.cmdNone)
      mapGame a (g, c) = (mapAppGame (always g) a, c)
      mapGameMsg gmsg stateMapper a = 
        case a of 
          Game c g -> 
            Game.update gmsg g 
            |> Tuple.mapBoth (Game c >> stateMapper) (Cmd.map GameMessage)
          _ -> (a, Cmd.none)
  in
  case msg of
    ViewOptions ->
      Options (appModel app) |> dropSFX
      |> withNone
    ViewCredits ->
      Credits (appModel app) |> dropSFX
      |> withNone
    ViewMainMenu ->
      Menu (appModel app) |> dropSFX
      |> withNone
    UpdateVideoSystem vmsg -> 
      updateVideo vmsg app
    UpdateConfig configMsg ->
      case configMsg of 
        (ListenForKey control) ->
          OptionsUpdatingControl (appModel app) (control)
          |> withNone
        SetMusicVolume volume ->
          mapAppConfig (Config.setMusicVolume volume) app
          |> withNone
        SetSFXVolume volume ->
          mapAppConfig (Config.setSFXVolume volume) app |> dropSFX
          |> withNone
    SoundLoaded (Ok sound) ->
      app 
      |> mapAppAudio (\a -> {a | music = Just sound})
      |> withNone
    SoundLoaded (Err e) ->
      let
          errr = Debug.log "Sound loading error" e
      in
      (app, Cmd.none, Audio.cmdNone)
      {-
    InitialValues t lvals ->
      app
        |> mapAppClock (always (Clock t))
        |> mapAppRandom ((++) lvals)
        |> mapAppGame (always (Game.newGameWithLemmings t Config.newConfig (Lemmings.newPopulationWithRandomValues lvals)))
        |> withNone -}
    Tick t ->
      case app of
        Loading m _ Nothing -> Loading m (Just t) Nothing |> withNone
        Loading m _ (Just v) -> newApp m t v |> withNone
        Game c g ->
          mapGameMsg (Game.Tick t) (mapAppClock (always (Clock t))) app
          |> withAudioNone
        _ -> mapAppClock (always (Clock t)) app |> withNone
    KeyUp k ->
      case app of 
        Game c g ->
          mapGameMsg (Game.KeyUp k) identity app
          |> withAudioNone
        _ ->
          app |> withNone
    KeyDown k ->
      case app of 
        OptionsUpdatingControl config control ->
          mapAppConfig (Config.updateControl control k) app |> appModel |> Options
          |> withNone
        Game _ _ ->
          mapGameMsg (Game.KeyDown k) identity app
          |> withAudioNone
        _ ->
          app |> withNone
    GameMessage (Game.RestartGame) ->
      ( app
      , Cmd.batch (List.map (uncurry Random.generate) generateLemmingValues)
      , Audio.cmdNone
      )
          {-
    GameMessage (Game.StartGame) ->
      ( Game 
        (appModel app) 
        ((Game.newGame (getAppClock app |> clockValue) (getAppConfig app))) 
        |> dropSFX
      , Cmd.batch (List.map (uncurry Random.generate) generateLemmingValues)
      , Audio.cmdNone
      )-}
    StartGame ->
      ( app
      , Cmd.batch (List.map (uncurry Random.generate) generateLemmingValues)
      , Audio.cmdNone
      )
    GameMessage (Game.GotLemmingValue l) ->
      case app of
        Loading m Nothing _ -> Loading m Nothing (Just l) |> withNone
        Loading m (Just t) _ -> newApp m t l |> withNone
        Game c g ->
          mapGameMsg (Game.NewGame (getAppConfig app) (getAppClock app |>  clockValue) l) (mapAppRandom (always l)) app
          |> withAudioNone
        _ -> app |> withNone
    GameMessage (Game.ExitGame) ->
      Menu (appModel app) |> dropSFX
      |> withNone
    GameMessage gmsg ->
      mapGameMsg gmsg identity app
      |> withAudioNone
    _ -> app |> withNone


updateVideo : VideoMsg -> App -> (App, Cmd Msg, Audio.AudioCmd Msg)
updateVideo vmsg model =
  case vmsg of
    SetWindowSize width height ->
      ( mapAppVideo (\v -> { v | width = width, height = height }) model
      , Cmd.none
      , Audio.cmdNone
      )
    SetFrameDelta delta ->
      ( mapAppVideo (\v -> { v | delta = delta }) model
      , Cmd.none
      , Audio.cmdNone
      )

subscriptions : Audio.AudioData -> App -> Sub Msg
subscriptions _ model = 
  let
      inputEvts = 
        case model of
          Game _ _ -> 
            [ Browser.Events.onKeyDown (Json.Decode.map KeyDown Key.decoder)
            , Browser.Events.onKeyUp (Json.Decode.map KeyUp Key.decoder)
            ]
          OptionsUpdatingControl _ _ ->
            [ Browser.Events.onKeyDown (Json.Decode.map KeyDown Key.decoder)
            , Browser.Events.onKeyUp (Json.Decode.map KeyUp Key.decoder)
            ]
          _ -> 
            []
      timingEvts = 
        case model of
          Game _ _ ->
            [ {-Browser.Events.onAnimationFrameDelta setFrameDelta
            , -}Time.every 100 (Game.Tick >> GameMessage)
            ]
          _ -> []

  in
    Browser.Events.onResize setWindowSize
    :: inputEvts
    ++ timingEvts
    |> Sub.batch

view : Audio.AudioData -> App -> Html.Html Msg
view _ app =
  let viewScreen = app |> getAppVideo |> video |> (\attrs og -> Html.main_ (og ++ attrs))
      config = getAppConfig app
  in
  case app of
    Loading _ _ _ ->
      Html.main_ [] []
    (Game _ g) as game -> 
      g
      |> Game.view
      |> viewScreen [ Hats.id "game" ]
      |> Html.map GameMessage
    (Title _) ->
      viewScreen []
        [ Html.h1 [] [ Html.text "Salvatore's Bridge" ] ]
    (Credits _) ->
      viewScreen [ Hats.id "credits" ]
        [ Html.h2 [] [ Html.text "Credits" ]
        , Html.h3 [] [ Html.text "Programming, Writing, Graphics" ]
        , Html.h4 [] [ Html.text "Zachariah" ]
        , Html.button [ Emits.onClick ViewMainMenu ] [ Html.text "Go Back" ]
        ]
    (Menu _ ) ->
      Html.h1 [] [ Html.text "Salvatore's Bridge" ]
        :: mainMenu
        |> viewScreen [ Hats.id "main-menu" ]
    (Options model) ->
      [ Html.h2 [] [ Html.text "Options" ]
      , (controllerOptions config)
      , (musicVolumeOptions config)
      ]
      ++ [ Html.button [ Emits.onClick ViewMainMenu ] [ Html.text "Go Back" ] ]
      |> viewScreen [ Hats.id "options" ]
    (OptionsUpdatingControl model control) ->
      [ Html.h2 [] [ Html.text "Options" ]
      , Html.h3 [] [ Html.text "Controls" ]
      , (controllerOptions config)
      , (musicVolumeOptions config)
      ]
      ++ [ Html.button [ Emits.onClick ViewMainMenu ] [ Html.text "Go Back" ] ]
      ++ (controlOverlay control)
      |> viewScreen [ Hats.id "options" ]

mainMenu = 
  [ ("New Game", StartGame)
  , ("Options", ViewOptions)
  , ("Credits", ViewCredits)
  ] 
  |> List.map (uncurry menuButton)


menuButton string msg =
  Html.button 
    [ Emits.onClick msg ]
    [ Html.text string ]

controllerOption config k =
  [ Html.dl [ Hats.class "controller-row" ]
    [ Html.dt  []
      [ Html.text (Config.nameForControl k)
      , Html.text ": "
      ]
      , Html.dd [] 
        [ Html.text (Config.keyFor k config |> KeyNames.keyNames)
        , Html.button [ Emits.onClick (UpdateConfig <| ListenForKey k)] [ Html.text "edit" ]
        ]
    ]
  ]

controllerOptions config =
  let 
      controlMap : Config.Control -> List (Html.Html Msg) -> List (Html.Html Msg)
      controlMap x acc=
        Tuple.pair x acc 
        |> Tuple.mapFirst (controllerOption config) 
        |> uncurry (flip (++)) 
  in
  List.foldl controlMap [] Config.allControls
  |> (++) [ Html.h3 [] [ Html.text "Controls" ] ]
  |> Html.section [ Hats.class "controls" ]

controlOverlay control =
  [ Html.div [ Hats.id "listening-overlay" ] [ "Press desired key for " ++ (Config.nameForControl control) |> Html.text ] ]

musicVolumeOptions config =
  Html.h3 [] [ Html.text "Volume" ]
    :: musicOption (Config.musicVolume config |> String.fromFloat)
    ++ sfxOption (Config.sfxVolume config |> String.fromFloat)
    |> Html.section [ Hats.class "music" ]

volumeOption name id msg val =
  [ Html.label [ Hats.for id ] [ Html.text name ]
  , Html.input 
    [ Hats.type_ "range"
    , Hats.id id
    , Hats.min "0.0"
    , Hats.max "1.0"
    , Hats.step "0.05"
    , Hats.value val
    , Emits.onInput (String.toFloat >> Maybe.withDefault 0.5 >> msg)
    ] []
  ]

musicOption = volumeOption "Music" "music-volume-slider" (UpdateConfig << SetMusicVolume)
sfxOption = volumeOption "SFX" "sfx-volume-slider" (UpdateConfig << SetSFXVolume)

