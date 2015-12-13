
module App where

import Html exposing (Html, div, button, text)
import Html.Events as Ev
import Start
import Effects exposing (Effects, Never)
import Task exposing (Task)

import History
import Native.MyUtils

import Other

stringify : a -> String
stringify = Native.MyUtils.stringify

main : Signal Html
main = app.html

type Action
  = ChangeLocation String
  | Click
  | GoTo
  | Nop

type alias Model =
  { click : Int
  , location : String
  }

app : Start.App Model
app = Start.start config

config : Start.Config Model Action
config =
  { init   = (model, Effects.none)
  , view   = view
  , update = update
  , inputs = [ Signal.map ChangeLocation History.path ]
  }

model : Model
model =
  {
    click = 100
  , location = ""
  }

view address model =
  Html.div [ ]
        [ Html.text (toString model)
        , Html.button [ Ev.onClick address Click ] [ Html.text Other.name ]
        , Html.button [ Ev.onClick address GoTo ] [ Html.text "go to" ]
        ]

update action model =
  case Debug.watchSummary "Action" stringify action of

    Click ->
      let newModel = { model | click = model.click + 1 }
      in (newModel, Effects.none)

    ChangeLocation newLocation ->
      let newModel = { model | location = newLocation }
      in (newModel, Effects.none)

    GoTo -> (model, Effects.map (always Nop) (Effects.task (History.setPath "/hi")))

    Nop -> (model, Effects.none)

port tasks : Signal (Task Never ())
port tasks = app.tasks
