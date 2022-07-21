module Message exposing (Msg(..))



type Msg 
  = NewRandomValues (List Float)
  | PauseGame
  | ResumeGame
  | QuitGame
  | StartGame


