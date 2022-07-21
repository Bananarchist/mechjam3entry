module Game exposing (Game, newGame, view, Msg(..), update, AudioSignal(..), audioSignals, newGameWithLemmings)

import Basics.Extra exposing (flip)
import Bridge
import Config 
import GFXAsset
import Html
import Html.Attributes as Hats
import Key
import Lemmings
import Mech
import Message
import Ocean
import Time

type Msg 
  = KeyUp Key.Key
  | KeyDown Key.Key
  | Tick Time.Posix
  | StartGame
  | RestartGame
  | ExitGame
  | GotLemmingValue (List Float)
  | NewGame Config.Config Time.Posix (List Float) 


type alias State =
  { mech : Mech.Model
  , bridge : Bridge.Bridge
  , score : Score
  , lastTick : Time.Posix
  , ocean : Ocean.Ocean
  , lemmings : List Lemmings.Lemming
  , lemmingRandomValues : List Float
  , audio : List AudioSignal
  }

newState : Time.Posix -> State
newState time = 
  State Mech.newMech Bridge.newBridge newScore time Ocean.new Lemmings.newPopulation [] []
newStateWithLemmings (vals, lemmings) time =
  State Mech.newMech Bridge.newBridge newScore time Ocean.new lemmings vals []

newStateWithPreservedLvals time lvals =
  newState time
  |> (\s -> { s | lemmingRandomValues = lvals })
  |> applyRandomValuesTo

applyRandomValuesTo state =
  let (remaining, lemmings) = Lemmings.mapRandomValues state.lemmings state.lemmingRandomValues
  in
  { state
  | lemmings = lemmings
  , lemmingRandomValues = remaining
  }


type Phase
  = Introduction
  | Play 
  | Ending GameResult
  | Transition Float Phase Phase

type GameResult
  = CompleteVictory Score
  | SavedPeople Score
  | Died Score
  | Lost

type AudioSignal 
  = StartMusic
  | PauseMusic

startMusic state = 
  { state | audio = StartMusic :: state.audio }

type Score = Score Float


type alias Debugger =
  {}

newScore = Score 100.0

decrScore (Score x) val = Score (x - val)

losingScore (Score x) = x < 0

type Game
  = Game Config.Config Phase State (Maybe Debugger)
  | Paused Game

defaultPhase = Play
newGame time config = Game config defaultPhase (newState time) Nothing
newGameWithLemmings lemmings time config =
  Game config defaultPhase (newStateWithLemmings lemmings time) Nothing 
unpause game =
  case game of
    Paused g -> unpause g
    _ -> game
isInTransition phase =
  case phase of
    Transition _ _ _ -> True
    _ -> False
mapTransitionTime updater phase =
  case phase of
    Transition x s e -> if updater x <= 0 then e else Transition (updater x) s e
    _ -> phase 
isPaused game =
  case game of
    Paused _ -> True
    _ -> False
updateState updater game = 
  case game of 
    Game config phase state maybeDebugger -> 
      Game config phase (updater state) maybeDebugger
    g -> unpause g |> updateState updater |> Paused
mapPhase updater game = 
  case game of 
    Game config phase state maybeDebugger -> 
      Game config (updater phase) state maybeDebugger
    _ -> game
gameConfig game =
  case game of
    Game config _ _ _ -> config
    Paused g -> gameConfig g
gamePhase game =
  case game of
    Game _ phase _ _ -> phase
    Paused g -> gamePhase g
gameState game =
  case game of
    Game _ _ state _ -> state
    Paused g -> gameState g
gameDebugger game =
  case game of
    Game _ _ _ maybeDebugger -> maybeDebugger
    Paused g -> gameDebugger g
audioSignals =
  gameState >> .audio

introTide = 0

canPlace mech ({start,mid,end}) =
  if not start && Mech.xPos mech < 34 then True
  else if not mid && Mech.xPos mech < 67 then True
  else if not end && Mech.xPos mech < 100 then True
  else False


scoreDepletionRate ({start,mid,end}, (Score s)) =
  [start,mid,end]
  |> List.foldl (\x xs -> if x then 1+xs else xs) 0
  |> toFloat
  |> flip (/) 3
  |> (*) (1 - s)
  |> (*) 0.0125


gameResult game =
  let
      config = gameConfig game
      phase = gamePhase game
      ({mech, bridge, lastTick, score, ocean, lemmings} as state) = gameState game
      maybeDebugger = gameDebugger game
  in
  case phase of
    Play -> 
      if Bridge.complete bridge && (not <| Lemmings.anyLeftToRescue lemmings) then
        if Mech.xPos mech >= 400 then
          Just (CompleteVictory score) |> Debug.log "Complete victory"
        else
          Just (SavedPeople score)|> Debug.log "saved people"
      else if Ocean.highTide ocean then
        Just (Died score)|> Debug.log "high tide"
      else if Mech.hasDrowned mech then
        Just (Died (Score 1))|> Debug.log "has drowned"
      else if Mech.hasBeenStruck mech then
        Just (Died (Score 1))|> Debug.log "has been struck"
      else
        Nothing
    _ -> Nothing

updateIfGameOver game =
  let
      config = gameConfig game
      phase = gamePhase game
      state = gameState game
      maybeDebugger = gameDebugger game
  in
  gameResult game
  |> Maybe.map (\r -> (Game config (Transition 4500 (Play) (Ending r)) state maybeDebugger))
  |> Maybe.withDefault game


finishedWithIntro game =
  case game of
    Paused g -> finishedWithIntro g
    Game c Introduction s dbg -> Game c (Transition 4500 Introduction Play) s dbg
    _ -> game


currentPhase phase =
  case phase of
    Transition _ x _ -> x
    x -> x

update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  let
      config = gameConfig game
      phase = gamePhase game
      ({mech, bridge, lastTick, score, ocean, lemmings} as state) = gameState game
      maybeDebugger = gameDebugger game
      withNone g = (g, Cmd.none)
  in
  case (msg, isPaused game) of
    (GotLemmingValue lvals, _) ->
      updateState (\s -> 
        { s 
        | lemmingRandomValues = s.lemmingRandomValues ++ lvals 
        }
        |> applyRandomValuesTo) game
        |> withNone
    (ExitGame, _) ->
      game
      |> withNone
    (StartGame, _) ->
      finishedWithIntro game
      |> Debug.log "Started state"
      |> withNone
    (NewGame conf tick lvals, _) ->
      newGameWithLemmings (Lemmings.newPopulationWithRandomValues lvals) tick conf
      |> withNone
    (RestartGame, _) ->
      game
      |> updateState (always (newState lastTick) >> startMusic)
      |> Debug.log "restarted state"
      |> withNone
      --Game config Play (newState lastTick |> startMusic) maybeDebugger
    (KeyUp _, False) -> 
      if (Mech.velocity mech) /= 0 then
        updateState (\s -> {s | mech = Mech.setVelocity 0 mech }) game
        |> withNone
      else
        game
        |> withNone
    (KeyDown key, False) ->
      if Config.matches Config.Pause key config then
        Paused game
        |> withNone
      else
        case (Config.matches Config.MoveLeft key config, Config.matches Config.MoveRight key config) of
          (True, _) ->
            updateState (\s -> {s | mech = mech |> Mech.setVelocity 1 |> Mech.setLeftFacing True}) game
            |> withNone
          (_, True) -> 
            updateState (\s -> {s | mech = mech |> Mech.setVelocity 1 |> Mech.setLeftFacing False}) game
            |> withNone
          _ ->
            case (Config.matches Config.PickUp key config
                  , Config.matches Config.PutDown key config
                  , Config.matches Config.Place key config) of
              (True, _, _) ->
                bridge
                |> Bridge.liftBridgeSegment 
                  (Mech.xPos mech)
                  (\b -> updateState (\s -> { s | mech = mech |> Mech.startLifting, bridge = b}) game)
                |> Maybe.withDefault game
                |> withNone
              (_, True, _) -> 
                updateState (\s -> {s | bridge = Bridge.dropBridgeSegment bridge }) game
                |> withNone
              (_, _, True) -> 
                updateState (\s -> {s | bridge = Bridge.placeBridgeSegment bridge }) game
                |> withNone
              _ -> 
                game
                |> withNone
    (KeyDown key, True) ->
      if Config.matches Config.Pause key config then
        unpause game
        |> withNone
      else
        game
        |> withNone
    (Tick tock, False) ->
        let newAudioSignals = audioSignals game
            delta = (Time.posixToMillis tock) - (Time.posixToMillis lastTick) |> toFloat
        in
        case currentPhase phase of
          Play ->
            let updatedMech = Mech.update state delta
                updatedBridge = Bridge.update (Mech.xPos updatedMech) delta bridge 
                updatedOcean = Ocean.update delta ocean
                newStateFn s = 
                  { s 
                    | lastTick = tock
                    , mech = updatedMech
                    , bridge = updatedBridge
                    , ocean = updatedOcean
                    , lemmings = Lemmings.update updatedBridge updatedOcean delta lemmings
                    , audio = newAudioSignals
                  }
                newGameModel = 
                  updateState newStateFn game
            in
            if lastTick == (Time.millisToPosix 0) then
              updateState (\s -> { s | lastTick = tock}) game
              |> mapPhase (mapTransitionTime (flip (-) delta))
              |> withNone
              --mapPhase (mapTransitionTime (flip (-) delta)) newGameModel
            else 
              updateIfGameOver newGameModel
              |> mapPhase (mapTransitionTime (flip (-) delta))
              |> withNone
          _ ->
            updateState (\s -> { s | lastTick = tock, audio = newAudioSignals}) game
            |> mapPhase (mapTransitionTime (flip (-) delta))
            |> withNone
    (Tick tock, True) ->
          updateState (\s -> { s | lastTick = tock}) game
          |> withNone
    (_, _) -> game
        |> withNone
          


-- views

background = GFXAsset.bg

view : Game -> List (Html.Html Msg)
view game =
  let
      config = gameConfig game
      {mech, bridge, lastTick, score, ocean, lemmings} = gameState game
      maybeDebugger = gameDebugger game
      pauseOverlay = if isPaused game then [ Html.div [ Hats.id "paused-overlay" ] [ Html.text "Game paused" ] ] else []
      transition c = flip (++) [Html.div [ Hats.class c ] []]
      (phase, withTransitionalView) = 
        case gamePhase game of
          Transition x Introduction Play -> 
            if x > 2000 then 
              (Introduction, transition "transition-fade reverse-animation") 
            else 
              (Play, transition "transition-fade")
          Transition x Play (Ending r) ->
            if x > 2000 then
              (Play, transition "transition-gradient")
            else
              (Ending r, transition "transition-fade")
          _ ->
            (gamePhase game, identity)
  in
  case phase of
    Introduction -> 
      [ "The storm was worse than anyone had predicted and made landfall almost a month ahead of hurricane season."
      , "No more boats were coming to evacuate us, they said."
      , "We shouldn't have lived in low-lying slums, they said."
      , "We fled our homes for the bridge, knowing it was incomplete, nearly impassible, our only chance."
      , "We would shimmy across the skeletal joists to safety. It was dangerous, but could be done."
      , "Scrappy a plan as we had, the rain-slick metal and howling wind rebuffed our hope. The tide was coming in. Tremendous waves crashed around us."
      , "I noticed below us, resting along the bluff, a construction mech and unplaced bridging panels."
      ]
      |> GFXAsset.fadeInText
      |> GFXAsset.withSendMessageBtn (StartGame) "Continue"
      |> withTransitionalView
    Ending result ->
      case result of 
        (CompleteVictory (Score s)) -> 
          [ "The officials named the bridge \"Port Bridge\" in that special, tone-deaf manor officials generally approach matters."
          , "The " ++ (String.fromFloat s) ++ " people who survived that storm, however, had a different name."
          , "We never did discover the identity of the pilot, of our savior. They disappeared into the crowd, and chose never to come forward taking credit for their triumph."
          , "I don't know who first came up with the name, or really what it means, but every year around this time, since I was a child, the " ++ (String.fromFloat s) ++ " of us make the pilgramage from near and far"
          , "To Salvatore's Bridge"
          ]
          |> GFXAsset.fadeInText
          |> GFXAsset.withSendMessageBtn (ExitGame) "The End"
          |> withTransitionalView
        (SavedPeople (Score s)) ->
          [ "The officials named the bridge \"Port Bridge\" in that special, tone-deaf manor officials generally approach matters."
          , "The " ++ (String.fromFloat s) ++ " people who survived that storm, however, had a different name."
          , "We never did discover the identity of the pilot, of our savior. They disappeared into the water after saving so many, unable to save themselves."
          , "I don't know who first came up with the name, or really what it means, but every year around this time, since I was a child, the " ++ (String.fromFloat s) ++ " of us make the pilgramage from near and far"
          , "To Salvatore's Bridge"
          ]
          |> GFXAsset.fadeInText
          |> GFXAsset.withSendMessageBtn (ExitGame) "The End"
          |> withTransitionalView
        _ ->
          [ "Our only hope had been lost to the water."
          , "Of a hundred people who stood on the bridge that night, praying, pleading..."
          , "I alone survived."
          , "I will take the details of my escape to the grave."
          , "I'm so sorry, mum."
          ]
          |> GFXAsset.fadeInText
          |> GFXAsset.withSendMessageBtn (RestartGame) "Try Again"
          |> GFXAsset.withSendMessageBtn (ExitGame) "Game Over"
          |> withTransitionalView
    _ -> 
      [ GFXAsset.distantBg
      , Ocean.viewBackTide ocean
      , Ocean.viewComingWave ocean
      ]
      ++ Bridge.viewPlaced bridge
      ++ Lemmings.view lemmings
      ++ [ GFXAsset.bg ]
      ++ Bridge.view bridge
      ++ Mech.view mech
      ++ Ocean.viewCrashingWave ocean
      ++ [ Ocean.viewFrontTide ocean ]
      ++ pauseOverlay
      |> withTransitionalView


