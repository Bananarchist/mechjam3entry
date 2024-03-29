module Game exposing (Game, newGame, view, Msg(..), update, AudioSignal(..), audioSignals, newGameWithLemmings)

import Basics.Extra exposing (flip, fractionalModBy)
import Bridge
import Config 
import Duration
import GFXAsset
import Html
import Html.Attributes as Hats
import Key
import Lemmings
import Mech
import Misc
import Ocean
import Sound
import Task
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
  | SoundTriggered Sound.Sound Time.Posix


type alias State =
  { mech : Mech.Model
  , bridge : Bridge.Bridge
  , score : Score
  , lastTick : Time.Posix
  , ocean : Ocean.Ocean
  , lemmings : List Lemmings.Lemming
  , lemmingRandomValues : List Float
  , audio : Maybe AudioSignal
  }

newState : Time.Posix -> State
newState time = 
  State Mech.newMech Bridge.newBridge newScore time Ocean.new Lemmings.newPopulation [] Nothing 
newStateWithLemmings (vals, lemmings) time =
  State Mech.newMech Bridge.newBridge newScore time Ocean.new lemmings vals Nothing


type AudioSignal =
  AudioSignal
    { sound : Sound.Sound
    , start : Time.Posix
    , next : Maybe AudioSignal
    }

newAudioSignal sound start =
  AudioSignal
    { sound = sound
    , start = start
    , next = Nothing
    }

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

getMusicForGame (game, cmds) =
  case (gamePhase game, gameState game) of
    (Transition x Introduction Play, _) ->
      updateState (\s -> {s | audio = Just (newAudioSignal (Sound.MusicLoop Sound.Intro) s.lastTick)}) game
      |> flip Tuple.pair (Task.perform (SoundTriggered (Sound.MusicLoop Sound.Intro)) Time.now)
    (Transition x Play (Ending (CompleteVictory _)), _) -> 
      updateState (\s -> {s | audio = Just (newAudioSignal (Sound.MusicLoop Sound.Outro) s.lastTick) }) game
      |> flip Tuple.pair (Task.perform (SoundTriggered (Sound.MusicLoop Sound.Outro)) Time.now)
    (Play, {ocean}) ->
      if (Ocean.oceanTide ocean |> Ocean.tidalValue) < 50 then
        updateState (\s -> {s | audio = Just (newAudioSignal (Sound.MusicLoop Sound.Light) s.lastTick)}) game
        |> flip Tuple.pair (Task.perform (SoundTriggered (Sound.MusicLoop Sound.Light)) Time.now)
      else
        updateState (\s -> {s | audio = Just (newAudioSignal (Sound.MusicLoop Sound.Frantic) s.lastTick) }) game
        |> flip Tuple.pair (Task.perform (SoundTriggered (Sound.MusicLoop Sound.Frantic)) Time.now)
    _ -> game |> flip Tuple.pair Cmd.none

shouldTransitionMusic : (Game, Cmd Msg) -> (Game, Cmd Msg)
shouldTransitionMusic (game, cmds) =
  let currentMusic = audioSignals game
      lastTick = gameState game |> .lastTick
      diff time duration =
        Time.posixToMillis time
        |> (-) (Time.posixToMillis lastTick)
        |> toFloat
        |> flip fractionalModBy (Duration.inMilliseconds duration)
  in
  currentMusic
  |> Maybe.map (\(AudioSignal ({sound, start, next} as cm)) ->
    if Maybe.map (\(AudioSignal n) -> Time.posixToMillis lastTick >= Time.posixToMillis n.start ) next
      |> Maybe.withDefault False
    then 
      Maybe.map (\(AudioSignal n) -> 
        updateState (\s -> { s | audio = next}) game
        |> flip Tuple.pair (Task.perform (always (SoundTriggered n.sound n.start)) Time.now)) next
      |> Maybe.withDefault (game, cmds)
    else
      case sound of 
        Sound.MusicLoop Sound.Intro -> 
          let d = diff start (Sound.loopLength Sound.Intro) 
              audio : AudioSignal
              audio = 
                AudioSignal 
                  { cm 
                    | next = Just 
                      ( newAudioSignal (Sound.MusicLoop Sound.Light) 
                        (d + (Time.posixToMillis lastTick |> toFloat) |> floor |> Time.millisToPosix))
                  }
          in
          updateState (\s -> { s | audio = Just audio }) game
          |> flip Tuple.pair cmds
        _ -> (game, cmds)
    )
    |> Maybe.withDefault (game, cmds)

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

type Score = Score Float


type alias Debugger =
  {}

newScore = Score 100.0

decrScore (Score x) val = Score (x - val)

losingScore (Score x) = x < 0

type Game
  = Game Config.Config Phase State (Maybe Debugger)
  | Paused Game

defaultPhase = Introduction
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
      survivors = List.length (List.filter Lemmings.isAlive lemmings) |> toFloat
      anySurvivors = survivors > 0
      realScore = survivors |> max 2 |> Score
  in
  case phase of
    Play -> 
      if Bridge.complete bridge && (not <| Lemmings.anyLeftToRescue lemmings) && anySurvivors then
        if Mech.xPos mech >= 400 then
          Just (CompleteVictory realScore) |> Debug.log "Complete victory"
        else
          Just (SavedPeople realScore)|> Debug.log "saved people"
      else if not anySurvivors then
          Just Lost
      else if Ocean.highTide ocean then
        Just (Died realScore)|> Debug.log "high tide"
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
      |> withNone
      |> getMusicForGame
    (NewGame conf tick lvals, _) ->
      newGameWithLemmings (Lemmings.newPopulationWithRandomValues lvals) tick conf
      |> withNone
    (RestartGame, _) ->
      game
      |> updateState (always (newState lastTick))
      |> withNone
      |> getMusicForGame
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
              |> shouldTransitionMusic
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
      warningScreen =
        if Ocean.oceanWave ocean |> Ocean.waveValue |> Misc.within 0 5 then
          [ GFXAsset.warning ]
        else 
          []
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
        Lost ->
          [ "The waves took everyone."
          , "The tide was too strong, all surfaces too slick."
          , "I made it, somehow..."
          ]
          |> GFXAsset.fadeInText
          |> GFXAsset.withSendMessageBtn (RestartGame) "Try Again"
          |> GFXAsset.withSendMessageBtn (ExitGame) "Game Over"
          |> withTransitionalView
        _ ->
          [ "Our only hope had been lost to the water."
          , "Of a hundred people who stood on the bridge that night, praying, pleading..."
          , "I alone survived."
          , "I pray not to revisit the details of my escape until Judgement Day."
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
      ++ Ocean.viewCrashingWave ocean
      ++ Mech.view mech
      ++ [ Ocean.viewFrontTide ocean ]
      ++ Mech.struckView mech
      ++ [ GFXAsset.rain ]
      ++ warningScreen
      ++ pauseOverlay
      |> withTransitionalView


