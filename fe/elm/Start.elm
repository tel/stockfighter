
module Start ( start, Config, App ) where

import Html exposing (Html)
import Task
import Effects exposing (Effects, Never)
import Signal.Extra exposing (foldp', mapMany, (<~))

{-| Define the semantics and display of the application in terms of a model and
a set of actions upon it. At the core, we consider the model as it changes in
time due to the application of actions against it (via update). The view
function does nothing more than "display" the state as the view of our web
application. It is given an Address so that the view may contain triggers to
update the model.

Atop this simple model rests two more notions used for "side effecting" world
interactions.

The first is the notion of a set of "effects" which can arise either upon
application startup (via init) or throughout the evolution of the model (via
update). These effects are interpreted in the main application as Tasks (see
App).

The second is the declaration of inputs which provides a set of signals the
application will listen in on. These are generated from "outside" the core
application and as soon as the application starts it begins to listen to their
events (in particular, their current values will be passed through the update
chain). -}
type alias Config model action =
  { init   : (model, Effects action)
  , update : action -> model -> (model, Effects action)
  , view   : Signal.Address action -> model -> Html
  , inputs : List (Signal.Signal action)
  }

{-| The description of a running App is provided by a trio of intertwined
Signals. The first, model, denotes the changing value of the application model
as it is transformed by actions (see Config). The second, tasks, is a stream of
tasks that the "outside" application is expected to complete in order to effect
the change desired by the App.

The third Signal, view, is just the application of the view function from Config
to the model signal.

Notably, the type of actions an App operates over has vanished. -}
type alias App model =
  { html  : Signal Html
  , model : Signal model
  , tasks : Signal (Task.Task Never ())
  }

{-| A "Transaction" is a set of actions performed "at once". Since Signals are
ultimately discrete this enables "bunching up" of actions. Used internally
alone. -}
type alias Transaction a = List a

{-| A single element Transaction. Most Transactions arise atomically. -}
atom : action -> Transaction action
atom action = [ action ]

{-| Merge two signals of transactions fairly, sequencing transactions if the
input signals update "simultaneously". -}
stepTogether :  Signal (Transaction a)
             -> Signal (Transaction a)
             -> Signal (Transaction a)
stepTogether = Signal.Extra.fairMerge List.append

{-| Construct an App from a Config. This function creates the internal signal
network that "closes the loop" between the view and the model. -}
start : Config model action -> App model
start config =
  let

    -- type alias St = (model, Effects action)
    --
    -- | The state, St, consists of the current model data alongside the set of
    -- effects to execute.

    -- type alias Step = Transaction action
    --
    -- | Given knowledge of the particular action we'll be using, we call a
    -- Transaction of actions a Step.

    -- : Signal.Mailbox Step
    steps = Signal.mailbox []

    -- : Signal.Address action
    address = Signal.forwardTo steps.address atom

    -- : action -> St -> St
    act action (oldModel, effects) =
      let (newModel, newEffects) = config.update action oldModel
      in (newModel, Effects.batch [ effects, newEffects ])

    -- : St -> (Step -> St)
    transact = List.foldl act

    -- : List (Signal Step)
    inputSteps = List.map (Signal.map atom) config.inputs

    -- : Signal Step
    allSteps = List.foldl stepTogether steps.signal inputSteps

    -- : Signal St
    effectsAndModel =
      foldp'
      (\actions (model, _) -> List.foldl act (model, Effects.none) actions)
      (List.foldl act config.init)
      allSteps

    -- : Signal model
    model = Signal.map fst effectsAndModel

    -- : St -> Task Never ()
    effectRunner (_, effects) = Effects.toTask steps.address effects

  in

    { model = model
    , html  = config.view address <~ model
    , tasks = effectRunner <~ effectsAndModel
    }
