module Statechart exposing
  ( Statechart
  , StateModel
  , decoder
  , encode
  , mkStatechart
  , mkCompoundState
  , mkState
  , noAction
  , empty
  , start
  , step
  )

{-| A pure Elm implementation of statecharts designed to hook direct;y into The Elm Architecture
update function.

@docs Statechart
@docs StateModel
@docs decoder
@docs encode
@docs mkStatechart
@docs mkCompoundState
@docs mkState
@docs noAction
@docs empty
@docs start
@docs step
-}

import Json.Encode as JE exposing (Value)
import Json.Decode as JD exposing (Decoder, field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded, custom)
import Json.Encode.Extra
import Update.Extra as Update
import Dict exposing (Dict)
import Tree exposing (Tree, singleton)
import Tree.Zipper exposing (fromTree, tree, findFromRoot, label, parent, children, firstChild, nextSibling)

{-|
-}
type alias Statechart msg model = Tree (StateConfig msg model)


type alias Zipper msg model = Tree.Zipper.Zipper (StateConfig msg model)


type alias Action msg model =
  model -> (model, List msg)


type alias BaseStateConfig msg model a =
  { a
  | name : String
  , onEnter : Action msg model
  , onExit : Action msg model
  , transition : msg -> model -> Maybe String
  }


type alias BranchStateConfig msg model = BaseStateConfig msg model
  { startingState : String
  , hasHistory : Bool
  }


type alias LeafStateConfig msg model = BaseStateConfig msg model {}


type StateConfig msg model
  = BranchStateConfig (BranchStateConfig msg model)
  | LeafStateConfig (LeafStateConfig msg model)


type alias Config msg model =
  { statechart : Statechart msg model
  , update : msg -> model -> (model, Cmd msg)
  , getStateModel : model -> StateModel
  , updateStateModel : (StateModel -> StateModel) -> model -> model
  }


{-|
-}
type alias StateModel =
  { currentStateName : String
  , targetStateName : String
  , history : Dict String String
  }


{-| Decode a stored StateModel.
-}
decoder : Statechart msg model -> Decoder StateModel
decoder statechart =
  decode StateModel
    |> required "currentStateName" JD.string
    |> required "targetStateName" JD.string
    |> required "history" (JD.dict JD.string)


{-| Encode the StateModel into a value ready to be stored in the main model.
-}
encode : StateModel -> Value
encode model =
  JE.object
    [ ("currentStateName", JE.string model.currentStateName)
    , ("targetStateName", JE.string model.targetStateName)
    , ("history", Json.Encode.Extra.dict identity JE.string model.history)
    ]


{-| Make the top level statechart
-}
mkStatechart : String -> List (Statechart msg model) -> Statechart msg model
mkStatechart startingState states =
  mkCompoundState
    { name = ""
    , onEnter = noAction
    , onExit = noAction
    , transition = \_ _ -> Nothing
    , startingState = startingState
    , hasHistory = False
    }
    states


{-| Make a compound state from a StateConfig
-}
mkCompoundState : BranchStateConfig msg model -> List (Statechart msg model) -> Statechart msg model
mkCompoundState branchStateConfig states =
  Tree.tree (BranchStateConfig branchStateConfig) states


{-| Make a state from a StateConfig
-}
mkState : LeafStateConfig msg model -> Statechart msg model
mkState leafStateConfig =
  singleton (LeafStateConfig leafStateConfig)


{-| The identity action that does nothing
-}
noAction : Action model msg
noAction model =
  (model, [])


{-| An empty StateModel; this is the initial value in the model.
-}
empty : StateModel
empty =
  { currentStateName = ""
  , targetStateName = ""
  , history = Dict.empty
  }


{-| Start the statechart based on the StateModel.
TODO: the problem here is that if the starting state has no children then onEnter doesn't get run
-}
start : Config msg model -> (model, Cmd msg) -> (model, Cmd msg)
start ({ statechart, update, getStateModel, updateStateModel } as config) ((model, cmd) as init) =
  let
    startingStateName =
      case Tree.label statechart of
        BranchStateConfig { startingState } -> startingState
        otherwise -> Debug.crash "This can't happen"
    
    startingState =
      unsafeFindState startingStateName statechart
  in
  init
    -- Update the model with the startingState
    |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = startingStateName, targetStateName = startingStateName }))
    -- In case the startingState isn't a compound state, we need to manually run any onEnter
    |> \init -> if (List.isEmpty (children startingState)) then applyAction update ((label >> onEnter) startingState) init else init
    -- Kick off the state machine
    |> moveTowardsTarget config


{-| Step through the statechart in response to an incoming message.
-}
step : Config msg model -> msg -> (model, Cmd msg) -> (model, Cmd msg)
step ({ statechart, update, getStateModel, updateStateModel } as config) msg ((model, cmd) as init) =
  let
    -- Get the current state from the model
    currentState : Zipper msg model
    currentState =
      unsafeFindState ((getStateModel >> .currentStateName) model) statechart

    -- Find the next state using the transition functions
    targetState : Zipper msg model
    targetState =
      getTransitionState msg model statechart currentState
        |> Maybe.withDefault currentState
  in
  init
    -- Update the model with the new targetState
    |> Update.updateModel (updateStateModel (\m -> { m | targetStateName = (label >> name) targetState }))
    -- Move to the target
    |> moveTowardsTarget config


{-| Move one node towards the target updating the model and running any actions
-}
moveTowardsTarget : Config msg model -> (model, Cmd msg) -> (model, Cmd msg)
moveTowardsTarget ({ statechart, update, getStateModel, updateStateModel } as config) ((model, cmd) as init) =
  let
    -- Get the current state from the model
    currentState : Zipper msg model
    currentState =
      unsafeFindState ((getStateModel >> .currentStateName) model) statechart

    -- Calculate the next step to go to, and determine any actions to run around the state change
    result : Maybe { beforeAction : Action msg model, nextState : Zipper msg model, targetState : Zipper msg model, afterAction : Action msg model }
    result =
      unsafeFindState ((getStateModel >> .targetStateName) model) statechart
        |> (\targetState ->
          if (currentState == targetState) then
            if List.isEmpty (children currentState) then
              -- If we have no children we are done and there is no next state
              Nothing
            else
              -- If this is a compound state then we need to go to its starting state (as we can't end in a compound state)
              targetState
                -- Get the starting state
                |> (\state -> case label state of
                  BranchStateConfig { name, startingState, hasHistory } ->
                    let
                      stateNameToStartIn =
                        if hasHistory then
                          (getStateModel >> .history) model
                            |> Dict.get name
                            |> Maybe.withDefault startingState
                        else
                          startingState
                    in
                    Just <| unsafeFindState stateNameToStartIn statechart

                  LeafStateConfig _ ->
                    Debug.crash "The builder functions shouldn't ever allow this to occur"
                )
                -- TODO: check that the starting state isn't the same as the targetState (as this would cause an infinite loop)
          else
            Just targetState
        )

        |> Maybe.andThen (\targetState ->
          if contains (tree targetState) (tree currentState) then
            -- We need to move one step down the tree (towards the target) and run the target's onEnter
            firstChild currentState
              |> Maybe.andThen (findSiblingContainingChild targetState)
              |> Maybe.map (\state ->
                { beforeAction = noAction
                , nextState = state
                , targetState = targetState
                , afterAction = (label >> onEnter) state
                })
          else
            let
              writeHistoryAction : Zipper msg model -> Zipper msg model -> Action msg model
              writeHistoryAction currentState parentState model =
                ( updateStateModel (\m -> { m | history = Dict.insert ((label >> name) parentState) ((label >> name) currentState) m.history }) model
                , []
                )
            in
            -- We need to move one step up the tree and run the current node's onExit
            parent currentState
              |> Maybe.map (\state ->
                { beforeAction = sequenceActions
                  [ writeHistoryAction currentState state
                  , (label >> onExit) currentState
                  ]
                , nextState = state
                , targetState = targetState
                , afterAction = noAction
                })
        )
  in
  case result of
    Just { beforeAction, nextState, targetState, afterAction } ->
      init
        -- Apply the before action (this will be an onExit if we are moving up the tree)
        |> applyAction update beforeAction
        -- Log the transition we are about to do
        |> \x -> let _ = Debug.log "State transition" (((getStateModel >> .currentStateName) model) ++ " -> " ++ ((label >> name) nextState)) in x
        -- Update the model with the new nextState and targetState
        |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = (label >> name) nextState, targetStateName = (label >> name) targetState }))
        -- Apply the after action (this will be an onEnter if we are moving down the tree)
        |> applyAction update afterAction
        -- Continue moving towards the target by recursing back into this function
        |> moveTowardsTarget config

    Nothing ->
      init


{-|
-}
unsafeFindState : String -> Statechart msg model -> Zipper msg model
unsafeFindState nameToFind statechart =
  let
    zipper =
      statechart
        |> fromTree
        |> findFromRoot (\stateConfig -> name stateConfig == nameToFind)
  in
  case zipper of
    Just z -> z
    Nothing -> Debug.crash <| "Attempt to transition to non-existent state " ++ nameToFind ++ ".  Check your statechart and transitions!"


{-| Helper function to say whether or not a parent contains a child
-}
contains : Statechart msg model -> Statechart msg model -> Bool
contains child parent =
  if (Tree.label >> name) child == (Tree.label >> name) parent then
    True
  else
    case Tree.children parent of
      [] -> False
      xs -> List.any (contains child) xs


{-| Helper function to move through siblings looking for the one that is, or contains the child
-}
findSiblingContainingChild : Zipper msg model -> Zipper msg model -> Maybe (Zipper msg model)
findSiblingContainingChild child currentSibling =
  if child == currentSibling || contains (tree child) (tree currentSibling) then
    Just currentSibling
  else
    currentSibling
      |> nextSibling
      |> Maybe.andThen (findSiblingContainingChild child)


{-| Calculate the target state using the transition function, going up the tree till we find one (or not)
-}
getTransitionState : msg -> model -> Statechart msg model -> Zipper msg model -> Maybe (Zipper msg model)
getTransitionState msg model statechart state =
  case ((label >> transition) state) msg model of
    Just name ->
      Just <| unsafeFindState name statechart

    Nothing ->
      state
        |> parent
        |> Maybe.andThen (getTransitionState msg model statechart)


{-| Apply an action to a model/command tuple
-}
applyAction : (msg -> model -> (model, Cmd msg)) -> Action msg model -> (model, Cmd msg) -> (model, Cmd msg)
applyAction update action (model, cmd) =
  let
    (newModel, msgs) =
      action model
  in
  newModel ! [ cmd ]
    |> Update.sequence update msgs


{-| Compose two actions together, one after another
-}
composeActions : Action msg model -> Action msg model -> Action msg model
composeActions g f =
  \m ->
    let
      (fM, fMsgs) =
        f m

      (gM, gMsgs) =
        g fM
    in
    (gM, fMsgs ++ gMsgs)


{-| Turn a list of actions into a single action by composing them all
-}
sequenceActions : List (Action msg model) -> Action msg model
sequenceActions actions =
  List.foldl composeActions noAction actions


{-| Get the state's name from the config
-}
name : StateConfig msg model -> String
name stateConfig =
  case stateConfig of
    BranchStateConfig { name } -> name
    LeafStateConfig { name } -> name


{-| Get the state's onEnter from the config
-}
onEnter : StateConfig msg model -> Action msg model
onEnter stateConfig =
  case stateConfig of
    BranchStateConfig { onEnter } -> onEnter
    LeafStateConfig { onEnter } -> onEnter


{-| Get the state's onExit from the config
-}
onExit : StateConfig msg model -> Action msg model
onExit stateConfig =
  case stateConfig of
    BranchStateConfig { onExit } -> onExit
    LeafStateConfig { onExit } -> onExit


{-| Get the state's transition from the config
-}
transition : StateConfig msg model -> msg -> model -> Maybe String
transition stateConfig =
  case stateConfig of
    BranchStateConfig { transition } -> transition
    LeafStateConfig { transition } -> transition
