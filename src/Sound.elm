module Sound exposing (..)

import Audio
import Basics.Extra exposing (uncurry)
import Duration
import Misc


type Sound
  = Music
  | Drop
  | MusicLoop MusicSection

type MusicSection
  = Intro
  | Light
  | Frantic
  | Outro

musicLoop section =
  let cfg = Audio.audioDefaultConfig
      once =
        { cfg | startAt = startAt section }
      looping =
        { cfg | startAt = startAt section, loop = Just { loopStart = startAt section, loopEnd = loopEnd section}}
  in
  case section of
    Intro -> once 
    Light -> looping 
    Frantic -> looping
    Outro -> once

loopEnd section =
  case section of
    Intro -> Duration.seconds 6.1
    Light -> Duration.seconds 19.05
    Frantic -> Duration.seconds 44.20
    Outro -> Duration.seconds 86.1

startAt section =
  case section of 
    Intro -> Duration.seconds 0
    Light -> loopEnd Intro
    Frantic -> loopEnd Light
    Outro -> loopEnd Frantic

loopLength =
  Misc.duple
  >> Tuple.mapBoth loopEnd startAt
  >> Tuple.mapBoth Duration.inMilliseconds Duration.inMilliseconds
  >> uncurry (-)
  >> Duration.milliseconds

audioLength sound =
  case sound of
    MusicLoop s -> loopLength s
    Drop -> Duration.seconds 0.6
    Music -> Duration.seconds 86

audioConfig sound =
  case sound of 
    MusicLoop s -> musicLoop s
    _ -> Audio.audioDefaultConfig


soundBaseVolume sound =
  case sound of
    Drop -> 0.5
    _ -> 0.5
