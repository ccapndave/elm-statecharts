module Statecharts exposing
  ( State
  , Model
  )

{-| Convenience functions for working with updates in Elm

@docs State
@docs Model
-}

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder, field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)

{-|
-}
type State model msg
  = State (StateConfig model msg)
  | CompoundState (StateConfig model msg) State (List State)


type alias Action model msg =
  model -> (model, List msg)


type alias StateConfig model msg =
  { name : String
  , onEnter : Action model msg
  , onExit : Action model msg
  , transition : msg -> model -> Maybe (State model msg)
  }


unwrapStateConfig : State model msg -> StateConfig model msg
unwrapStateConfig state =
  case state of
    State stateConfig ->
      stateConfig
    
    CompoundState stateConfig _ _ ->
      stateConfig


{-|
-}
type alias Model model msg =
  { currentState : State model msg
  -- TODO: history
  }


encode : Model model msg -> Value
encode model =
  JE.object
    [ ("currentState", JE.string <| (unwrapStateConfig >> .name) model.currentState)
    ]


{-| Make a state from a StateConfig
-}
mkState : StateConfig model msg -> State model msg
mkState =
  State


{-| Make a compound state from a StateConfig
-}
mkCompoundState : StateConfig model msg -> State -> List State -> State model msg
mkCompoundState =
  CompoundState


{-
module FSM exposing
  ( State
  , StateConfig
  , mkState
  , noAction
  , start
  , step
  , decoder
  , encode
  )

{-| Convenience functions for working with updates in Elm

@docs State
@docs mkState
@docs noAction
@docs start
@docs step
@docs decoder
@docs encode
-}


import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder, field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)
import Update.Extra as Update
import Update.Extra.Infix exposing ((:>))
import List.Extra

{-|
-}
type State model msg
  = State (StateConfig model msg)


type alias Action model msg =
  model -> (model, List msg)


type alias StateConfig model msg =
  { name : String
  , onEnter : Action model msg
  , onExit : Action model msg
  , transition : msg -> model -> Maybe (State model msg)
  }


unwrap : State model msg -> StateConfig model msg
unwrap (State stateConfig) =
  stateConfig


{-| Decode a JSON representation of a state (simply its name) by looking it up in a list of states
-}
decoder : List (State model msg) -> Decoder (State model msg)
decoder states =
  JD.string
    |> JD.andThen (\name ->
      states
        |> List.Extra.find (\(State stateConfig) -> stateConfig.name == name)
        |> Maybe.map JD.succeed
        |> Maybe.withDefault (JD.fail <| "Unable to find a state named '" ++ name ++ "'")
    )


{-| Encode a state as a string (simply its name)
-}
encode : State model msg -> Value
encode (State { name }) =
  JE.string name


{-| Make a State from a StateConfig
-}
mkState : StateConfig model msg -> State model msg
mkState =
  State


{-| The identity action that does nothing
-}
noAction : Action model msg
noAction model =
  (model, [])


{-|
-}
start : (msg -> model -> (model, Cmd msg)) -> State model msg -> (model, Cmd msg) -> (model, Cmd msg)
start update startingState init =
  init
    |> applyAction update (enterState startingState)


{-|
-}
step
  : { update : msg -> model -> (model, Cmd msg)
    , getState : model -> State model msg
    , updateState : State model msg -> model -> model
    }
  -> msg
  -> (model, Cmd msg) -> (model, Cmd msg)
step { update, getState, updateState } msg ((model, cmd) as init) =
  let
    currentState =
      getState model

    { name, transition } =
      unwrap currentState

    -- Determine the next state (if the next state is nothing then set it to the current state)
    nextState : State model msg
    nextState =
      transition msg model
        |> Maybe.withDefault currentState
  in
  if (equals currentState nextState) then
    init
  else
    let
      _ = Debug.log "step" <| name ++ " -> " ++ (case nextState of State s -> s.name)
    in
    init
      |> applyAction update (exitState currentState)
      |> Update.updateModel (updateState nextState)
      |> applyAction update (enterState nextState)


{-| Compares two states.  Equality is based solely on a state's name.
-}
equals : State model msg -> State model msg -> Bool
equals (State stateConfig1) (State stateConfig2) =
  stateConfig1.name == stateConfig2.name


{-| Return the updated model and list of messages when exiting a state
-}
exitState : State model msg -> Action model msg
exitState (State { onExit }) model =
  onExit model


{-| Return the updated model and list of messages when entering a state
-}
enterState : State model msg -> Action model msg
enterState (State { onEnter }) model =
  onEnter model


{-| Apply an action to a model/command tuple
-}
applyAction : (msg -> model -> (model, Cmd msg)) -> Action model msg -> (model, Cmd msg) -> (model, Cmd msg)
applyAction update action (model, cmd) =
  let
    (newModel, msgs) =
      action model
  in
  newModel ! [ cmd ]
    |> Update.sequence update msgs

-}