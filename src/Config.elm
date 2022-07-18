module Config exposing (Config, newConfig, keyFor, matches, Control(..), allControls, updateControl, configNameForControl)

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
  configKeyForControl control |> fetcher


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

configKeyForControl control =
  case control of
    MoveLeft -> "moveLeft"
    MoveRight -> "moveRight"
    PickUp ->  "pickUp"
    PutDown -> "putDown"
    Place -> "place"
    Pause -> "pause"

configNameForControl control = 
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
    Dict.insert (configKeyForControl control) (ControllerKey control newKey) config
  else
    config

