module Main exposing (main)

import Html
import Browser
import Browser.Events
import Browser.Dom
import Task

main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Model
  = Model Video

type alias Video =
  { width : Int
  , height : Int
  , delta : Float
  }

videoWidth {width} = width
videoHeight {height} = height
videoFrameDelta {delta} = delta

videoSystem : Model -> Video
videoSystem (Model video) = video

type Msg
  = UpdateVideoSystem VideoMsg

type VideoMsg 
  = SetWindowSize Int Int
  | SetFrameDelta Float

setWindowSize w h = UpdateVideoSystem <| SetWindowSize w h
setFrameDelta = UpdateVideoSystem << SetFrameDelta


init : () -> (Model, Cmd Msg)
init _ =
    ( Model (Video 0 0 0.0)
    , Browser.Dom.getViewport |> Task.attempt (\r ->
            case r of
                    Ok vp ->
                            setWindowSize (floor vp.viewport.width) (floor vp.viewport.height)
                    Err e ->
                            setWindowSize 0 0
            )
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateVideoSystem vmsg -> 
      updateVideo vmsg model

updateVideo : VideoMsg -> Model -> (Model, Cmd Msg)
updateVideo vmsg model =
  let 
      video = videoSystem model
  in
  case vmsg of
    SetWindowSize width height ->
      ( Model ({ video | width = width, height = height })
      , Cmd.none
      )
    SetFrameDelta delta ->
      ( Model ({ video | delta = delta })
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Sub.batch
    [ Browser.Events.onAnimationFrameDelta setFrameDelta
    , Browser.Events.onResize setWindowSize
    ]

view : Model -> Html.Html Msg
view _ = Html.main_ [] []
