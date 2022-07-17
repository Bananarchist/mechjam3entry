module Config exposing (Config, newConfig, keyFor, matches, Control(..))

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
  case control of
    MoveLeft -> fetcher "moveLeft"
    MoveRight -> fetcher "moveRight"
    PickUp -> fetcher "pickUp"
    PutDown -> fetcher "putDown"
    Place -> fetcher "place"
    Pause -> fetcher "pause"


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


matches : Control -> Key.Key -> Config -> Bool
matches control key = keyFor control >> ((==) key) 

