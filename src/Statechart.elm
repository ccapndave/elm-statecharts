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
import Maybe.Extra as Maybe
import Dict exposing (Dict)
import Tree exposing (Tree, singleton)
import Tree.Zipper exposing (fromTree, tree, findFromRoot, label, parent, children, firstChild, nextSibling)

{-|
-}
type alias Statechart msg model = Tree (StateConfig msg model)


type alias Zipper msg model = Tree.Zipper.Zipper (StateConfig msg model)


type alias Action msg model =
  Maybe msg -> model -> (model, List msg)


type alias BaseStateConfig msg model a =
  { a
  | name : String
  , onEnter : Action msg model
  , onExit : Action msg model
  , transition : msg -> model -> Maybe String
  }


type alias BranchStateConfig msg model = BaseStateConfig msg model
  { startingState : Statechart msg model
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
decoder : Decoder StateModel
decoder =
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
mkStatechart : Statechart msg model -> List (Statechart msg model) -> Statechart msg model
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
  let
    -- Get the name of the starting state
    startingState =
      (Tree.label >> name) branchStateConfig.startingState

    -- We can do a little runtime check here to confirm that the starting state is a child of states
    isStartingStateChild =
      states
        |> List.map (Tree.label >> name)
        |> List.filter ((==) startingState)
        |> not << List.isEmpty
  in
  if isStartingStateChild then
    Tree.tree (BranchStateConfig branchStateConfig) states
  else
    Debug.crash <| "The starting state '" ++ startingState ++ "' is not a child of '" ++ branchStateConfig.name ++ "'"


{-| Make a state from a StateConfig
-}
mkState : LeafStateConfig msg model -> Statechart msg model
mkState leafStateConfig =
  singleton (LeafStateConfig leafStateConfig)


{-| The identity action that does nothing
-}
noAction : Action model msg
noAction msg model =
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
-}
start : Config msg model -> (model, Cmd msg) -> (model, Cmd msg)
start ({ statechart, update, getStateModel, updateStateModel } as config) ((model, cmd) as init) =
  let
    -- startingStateName =
    --   case Tree.label statechart of
    --     BranchStateConfig { startingState } -> startingState
    --     otherwise -> Debug.crash "This can't happen"
    --
    -- startingState =
    --   unsafeFindState startingStateName statechart

    startingState =
      case Tree.label statechart of
        BranchStateConfig { startingState } -> startingState
        otherwise -> Debug.crash "This can't happen"

    startingStateName =
      (Tree.label >> name) startingState
  in
  init
    -- Update the model with the startingState
    |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = startingStateName, targetStateName = startingStateName }))
    -- In case the startingState isn't a compound state, we need to manually run any onEnter actions
    |> \init -> if (List.isEmpty (Tree.children startingState)) then applyAction update ((Tree.label >> onEnter) startingState) Nothing init else init
    -- Kick off the state machine
    |> moveTowardsTarget config Nothing


{-| Step through the statechart in response to an incoming message.
-}
step : Config msg model -> msg -> (model, Cmd msg) -> (model, Cmd msg)
step ({ statechart, update, getStateModel, updateStateModel } as config) msg ((model, cmd) as init) =
  let
    -- Get the current state from the model
    currentState : Zipper msg model
    currentState =
      unsafeFindState ((getStateModel >> .currentStateName) model) statechart

    -- Find the next state using the transition functions, if there is one
    maybeTargetState : Maybe (Zipper msg model)
    maybeTargetState =
      getTransitionState msg model statechart currentState

    isSelfTransition : Bool
    isSelfTransition =
      maybeTargetState == (Just currentState)
  in
  init
    |> (\i -> if isSelfTransition then moveToParent config msg i else i)
    -- Update the model with the new targetState
    |> Update.updateModel (updateStateModel (\m -> { m | targetStateName = maybeTargetState |> Maybe.withDefault currentState |> (label >> name) }))
    -- Move to the target
    |> moveTowardsTarget config (Just msg)


{-| Move one node towards the target updating the model and running any actions
-}
moveTowardsTarget : Config msg model -> Maybe msg -> (model, Cmd msg) -> (model, Cmd msg)
moveTowardsTarget ({ statechart, update, getStateModel, updateStateModel } as config) maybeMsg ((model, cmd) as init) =
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
                  BranchStateConfig config ->
                    let
                      -- Choosing a starting state uses the following algorithm:
                      --  - a local transition always takes priority
                      --  - otherwise we get the starting state from history, if it is on and there is a stored state
                      --  - otherwise we use the default defined in the statechart
                      stateNameToStartIn =
                        maybeMsg
                          -- first try local transition, disallowed transitions back to the same state
                          |> Maybe.andThen (\msg -> getTransitionState msg model statechart state)
                          |> Maybe.map (label >> name)
                          |> Maybe.andThen (\transitionStateName -> if transitionStateName == config.name then Nothing else Just transitionStateName)

                          -- then try history
                          |> Maybe.or (
                              if config.hasHistory then
                                (getStateModel >> .history) model
                                  |> Dict.get config.name
                              else
                                Nothing
                             )

                          -- finally fall back to the default starting states
                          |> Maybe.withDefault ((Tree.label >> name) config.startingState)
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
            let
              writeHistoryAction : Zipper msg model -> Zipper msg model -> Action msg model
              writeHistoryAction currentState parentState = \_ model ->
                ( updateStateModel (\m -> { m | history = Dict.insert ((label >> name) parentState) ((label >> name) currentState) m.history }) model
                , []
                )
            in
            firstChild currentState
              |> Maybe.andThen (findSiblingContainingChild targetState)
              |> Maybe.map (\state ->
                { beforeAction = noAction
                , nextState = state
                , targetState = targetState
                , afterAction = sequenceActions
                  [ writeHistoryAction state currentState
                  , (label >> onEnter) state
                  ]
                })
          else
            -- We need to move one step up the tree and run the current node's onExit
            parent currentState
              |> Maybe.map (\state ->
                { beforeAction = sequenceActions
                  [ (label >> onExit) currentState
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
        |> applyAction update beforeAction maybeMsg
        -- Log the transition we are about to do
        |> \x -> let _ = Debug.log "State transition" (((getStateModel >> .currentStateName) model) ++ " -> " ++ ((label >> name) nextState) {- ++ " [" ++ toString maybeMsg ++ "]" -}) in x
        -- Update the model with the new nextState and targetState
        |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = (label >> name) nextState, targetStateName = (label >> name) targetState }))
        -- Apply the after action (this will be an onEnter if we are moving down the tree)
        |> applyAction update afterAction maybeMsg
        -- Continue moving towards the target by recursing back into this function
        |> moveTowardsTarget config maybeMsg

    Nothing ->
      init


{-| Move one node up the hierarchy updating the model and running any exit action (this is used in self-transitions)
-}
moveToParent : Config msg model -> msg -> (model, Cmd msg) -> (model, Cmd msg)
moveToParent ({ statechart, update, getStateModel, updateStateModel } as config) msg ((model, cmd) as init) =
  let
    -- Get the current state from the model
    currentState : Zipper msg model
    currentState =
      unsafeFindState ((getStateModel >> .currentStateName) model) statechart

    result =
      parent currentState
        |> Maybe.map (\state ->
          { beforeAction = sequenceActions
            [ (label >> onExit) currentState
            ]
          , nextState = state
          })
  in
  case result of
    Just { beforeAction, nextState } ->
      init
        -- Apply the before action (this will be an onExit if we are moving up the tree)
        |> applyAction update beforeAction (Just msg)
        -- Log the transition we are about to do
        |> \x -> let _ = Debug.log "State transition (moveToParent) " (((getStateModel >> .currentStateName) model) ++ " -> " ++ ((label >> name) nextState)) in x
        -- Update the model with the new nextState and targetState
        |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = (label >> name) nextState }))

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

applyAction : (msg -> model -> (model, Cmd msg)) -> Action msg model -> Maybe msg -> (model, Cmd msg) -> (model, Cmd msg)
applyAction update action maybeMsg (model, cmd) =
  let
    (newModel, msgs) =
      action maybeMsg model -- TODO
  in
  newModel ! [ cmd ]
    |> Update.sequence update msgs


{-| Compose two actions together, one after another
-}
composeActions : Action msg model -> Action msg model -> Action msg model
composeActions g f =
  \msg model ->
    let
      (fModel, fMsgs) =
        f msg model

      (gModel, gMsgs) =
        g msg fModel
    in
    (gModel, fMsgs ++ gMsgs)


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
