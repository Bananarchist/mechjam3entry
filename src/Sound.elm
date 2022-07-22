module Sound exposing (..)

import Audio
import Duration


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
  case section of
    Intro -> Audio.PlayAudioConfig Nothing 1 (Duration.seconds 0)
    Light -> Audio.PlayAudioConfig Nothing 1 (Duration.seconds 5)
    Frantic -> Audio.PlayAudioConfig Nothing 1 (Duration.seconds 20)
    Outro -> Audio.PlayAudioConfig Nothing 1 (Duration.seconds 30)

audioConfig sound =
  case sound of 
    MusicLoop s -> musicLoop s
    _ -> Audio.audioDefaultConfig
