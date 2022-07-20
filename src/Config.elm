module Config exposing (Config, newConfig, keyFor, matches, Control(..), allControls, updateControl, nameForControl, sfxVolume, setSFXVolume, musicVolume, setMusicVolume)

import Key
import Dict 

type alias Config = Dict.Dict String Option

type Control
  = MoveLeft
  | MoveRight
  | PickUp
  | PutDown
  | Place
  | Pause

allControls =
  [ MoveLeft
  , MoveRight
  , PickUp
  , PutDown
  , Place
  , Pause
  ]

type Option
  = ControllerKey Control Key.Key
  | MusicVolume Float
  | SFXVolume Float
  | Difficulty Float

newConfig : Config
newConfig = 
  Dict.empty

keyFor : Control -> Config -> Key.Key
keyFor control config =
  let fetcher k = Dict.get k config |> Maybe.andThen controllerKey |> Maybe.withDefault (defaultKey control) 
  in
  keyForControl control |> fetcher


defaultKey control = 
  case control of
    MoveLeft -> Key.ArrowLeft
    MoveRight -> Key.ArrowRight
    PickUp -> Key.ArrowUp
    PutDown -> Key.ArrowDown
    Place -> Key.Space
    Pause -> Key.Enter

controllerKey option =
  case option of
    ControllerKey c k -> Just k
    _ -> Nothing

keyForControl control =
  case control of
    MoveLeft -> "moveLeft"
    MoveRight -> "moveRight"
    PickUp ->  "pickUp"
    PutDown -> "putDown"
    Place -> "place"
    Pause -> "pause"

nameForControl control = 
  case control of
    MoveLeft -> "Move mech left"
    MoveRight -> "Move mech right"
    PickUp ->  "Pick up highlighted item"
    PutDown -> "Put down held item"
    Place -> "Place held item on bridge"
    Pause -> "Pause game"


matches : Control -> Key.Key -> Config -> Bool
matches control key = keyFor control >> ((==) key) 

anyUsingKey key config =
  Dict.filter 
    (\_ opt ->
      case opt of
        ControllerKey _ k -> if k == key then True else False
        _ -> False
    ) 
    config
    |> Dict.size
    |> (<) 0

updateControl control newKey config =
  if not <| anyUsingKey newKey config then
    Dict.insert (keyForControl control) (ControllerKey control newKey) config
  else
    config

defaultMusicVolume = 0.5
musicVolumeKey = "musicVolume"
musicVolume = Dict.get musicVolumeKey >> Maybe.map musicVolumeValue >> Maybe.withDefault defaultMusicVolume 
setMusicVolume = MusicVolume >> Dict.insert musicVolumeKey
musicVolumeValue opt =
  case opt of
    MusicVolume f -> f
    _ -> defaultMusicVolume

defaultSFXVolume = 0.5
sfxVolumeKey = "sfxVolume"
sfxVolume = Dict.get sfxVolumeKey >> Maybe.map sfxVolumeValue >> Maybe.withDefault defaultSFXVolume
setSFXVolume = SFXVolume >> Dict.insert sfxVolumeKey
sfxVolumeValue opt =
  case opt of
    SFXVolume f -> f
    _ -> defaultSFXVolume


