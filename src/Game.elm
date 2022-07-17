module Game exposing (Game, newGame, view, Msg(..), update)

import Basics.Extra exposing (flip)
import Bridge
import Config 
import GFXAsset
import Html
import Key
import Lemmings
import Mech
import Ocean
import Time

type Msg 
  = KeyUp Key.Key
  | KeyDown Key.Key
  | Tick Time.Posix
  | StartGame
  | RestartGame
  | ExitGame


type alias State =
  { mech : Mech.Model
  , bridge : Bridge.Bridge
  , score : Score
  , lastTick : Time.Posix
  , ocean : Ocean.Ocean
  , lemmings : List Lemmings.Lemming
  }

newState time = State Mech.newMech Bridge.newBridge newScore time Ocean.new Lemmings.newPopulation

type Phase
  = Introduction
  | Play 
  | Ending GameResult
  | Transition Phase Phase

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

newGame config time = Game config Introduction (newState time) Nothing
updateState updater (Game config phase state maybeDebugger) = Game config phase (updater state) maybeDebugger
mapPhase updater (Game config phase state maybeDebugger) = Game config (updater phase) state maybeDebugger

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


-- views

background = GFXAsset.bg

view : Game -> List (Html.Html Msg)
view ((Game config phase {mech, bridge, lastTick, score, ocean, lemmings} maybeDebugger) as game) =
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
        (SavedPeople (Score s)) ->
          [ "The officials named the bridge \"Port Bridge\" in that special, tone-deaf manor officials generally approach matters."
          , "The " ++ (String.fromFloat s) ++ " people who survived that storm, however, had a different name."
          , "We never did discover the identity of the pilot, of our savior. They disappeared into the water after saving so many, unable to save themselves."
          , "I don't know who first came up with the name, or really what it means, but every year around this time, since I was a child, the " ++ (String.fromFloat s) ++ " of us make the pilgramage from near and far"
          , "To Salvatore's Bridge"
          ]
          |> GFXAsset.fadeInText
          |> GFXAsset.withSendMessageBtn (ExitGame) "The End"
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
    _ -> 
      [ Ocean.viewBackTide ocean
      , Ocean.viewWave ocean
      , GFXAsset.bg
      ]
      ++ Lemmings.view lemmings
      ++ Bridge.view bridge
      ++ [ Mech.view mech ]
      ++ [ Ocean.viewFrontTide ocean ]

gameResult (Game _ phase ({mech,bridge,score,ocean,lemmings} as state) _) =
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

updateIfGameOver ((Game config phase state maybeDebugger) as game) =
  gameResult game
  |> Maybe.map (\r -> (Game config (Ending r) state maybeDebugger))
  |> Maybe.withDefault game



update : Msg -> Game -> Game
update msg ((Game config phase ({mech, bridge, score, lastTick, ocean, lemmings} as state) maybeDebugger) as game) =
  case msg of
    ExitGame ->
      game
    StartGame ->
      Game config Play (newState lastTick) maybeDebugger
    RestartGame ->
      Game config Play (newState lastTick) maybeDebugger
    KeyUp _ -> 
      if (Mech.velocity mech) /= 0 then
        updateState (\s -> {s | mech = Mech.setVelocity 0 mech }) game
      else
        game
    KeyDown key ->
      case (Config.matches Config.MoveLeft key config, Config.matches Config.MoveRight key config) of
        (True, _) ->
          updateState (\s -> {s | mech = mech |> Mech.setVelocity 1 |> Mech.setLeftFacing True}) game
        (_, True) -> 
          updateState (\s -> {s | mech = mech |> Mech.setVelocity 1 |> Mech.setLeftFacing False}) game
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
            (_, True, _) -> 
              updateState (\s -> {s | bridge = Bridge.dropBridgeSegment bridge }) game
            (_, _, True) -> 
              updateState (\s -> {s | bridge = Bridge.placeBridgeSegment bridge }) game
            _ -> 
              game
    Tick tock ->
      case phase of
        Play ->
          let delta = (Time.posixToMillis tock) - (Time.posixToMillis state.lastTick) |> toFloat
              updatedMech = Mech.update state delta
              updatedBridge = Bridge.update (Mech.xPos updatedMech) delta bridge 
              updatedOcean = Ocean.update delta ocean
              newStateFn s = 
                { s 
                  | lastTick = tock
                  , mech = updatedMech
                  , bridge = updatedBridge
                  , ocean = updatedOcean
                  , lemmings = Lemmings.update updatedBridge updatedOcean delta lemmings
                }
              newGameModel = 
                updateState newStateFn game
          in
          if lastTick == (Time.millisToPosix 0) then
            updateState (\s -> { s | lastTick = tock}) game
          else 
            updateIfGameOver newGameModel
        _ ->
          updateState (\s -> { s | lastTick = tock}) game
          


