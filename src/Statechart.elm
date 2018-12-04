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

{-| A pure Elm implementation of statecharts designed to hook directy into The Elm Architecture
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

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, field, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Maybe.Extra as Maybe
import Tree exposing (Tree, singleton)
import Tree.Zipper exposing (children, findFromRoot, firstChild, fromTree, label, nextSibling, parent, tree)
import Update.Extra as Update


{-| -}
type alias Statechart msg model =
    Tree (StateConfig msg model)


type alias Zipper msg model =
    Tree.Zipper.Zipper (StateConfig msg model)


type alias Action msg model =
    Maybe msg -> model -> ( model, List msg )


type alias BaseStateConfig msg model a =
    { a
        | name : String
        , onEnter : Action msg model
        , onExit : Action msg model
        , transition : msg -> model -> Maybe String
    }


type alias BranchStateConfig msg model =
    BaseStateConfig msg
        model
        { startingState : Statechart msg model
        , hasHistory : Bool
        }


type alias LeafStateConfig msg model =
    BaseStateConfig msg model {}


type StateConfig msg model
    = BranchStateConfig (BranchStateConfig msg model)
    | LeafStateConfig (LeafStateConfig msg model)


type alias Config msg model =
    { statechart : Statechart msg model
    , update : msg -> model -> ( model, Cmd msg )
    , getStateModel : model -> StateModel
    , updateStateModel : (StateModel -> StateModel) -> model -> model
    }


{-| -}
type alias StateModel =
    { currentStateName : String
    , targetStateName : String
    , history : Dict String String
    }


type alias ActionsAndStates msg model =
    { beforeAction : Action msg model
    , nextState : Zipper msg model
    , targetState : Zipper msg model
    , afterAction : Action msg model
    }


{-| Decode a stored StateModel.
-}
decoder : Decoder StateModel
decoder =
    succeed StateModel
        |> required "currentStateName" JD.string
        |> required "targetStateName" JD.string
        |> required "history" (JD.dict JD.string)


{-| Encode the StateModel into a value ready to be stored in the main model.
-}
encode : StateModel -> Value
encode model =
    JE.object
        [ ( "currentStateName", JE.string model.currentStateName )
        , ( "targetStateName", JE.string model.targetStateName )
        , ( "history", JE.dict identity JE.string model.history )
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
        startingStateName : String
        startingStateName =
            (Tree.label >> name) branchStateConfig.startingState
    in
    Tree.tree (BranchStateConfig branchStateConfig) states


{-| Make a state from a StateConfig
-}
mkState : LeafStateConfig msg model -> Statechart msg model
mkState leafStateConfig =
    singleton (LeafStateConfig leafStateConfig)


{-| The identity action that does nothing
-}
noAction : Action model msg
noAction msg model =
    ( model, [] )


{-| An empty StateModel; this is the initial value in the model.
-}
empty : StateModel
empty =
    { currentStateName = ""
    , targetStateName = ""
    , history = Dict.empty
    }


{-| Start the statechart in its starting state. This can fail in the case of a badly configured statechart.
-}
start : Config msg model -> ( model, Cmd msg ) -> Result String ( model, Cmd msg )
start ({ statechart, update, getStateModel, updateStateModel } as config) (( model, cmd ) as init) =
    case Tree.label statechart of
        BranchStateConfig branchStateConfig ->
            let
                startingState : Statechart msg model
                startingState =
                    branchStateConfig.startingState

                startingStateName : String
                startingStateName =
                    (Tree.label >> name) startingState

                isStartingStateChild : Bool
                isStartingStateChild =
                    Tree.children statechart
                        |> List.map (Tree.label >> name)
                        |> List.filter ((==) startingStateName)
                        |> (not << List.isEmpty)
            in
            if isStartingStateChild then
                init
                    -- Update the model with the startingState
                    |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = startingStateName, targetStateName = startingStateName }))
                    -- In case the startingState isn't a compound state, we need to manually run any onEnter actions
                    |> (\updatedInit ->
                            if List.isEmpty (Tree.children startingState) then
                                Ok <| applyAction update ((Tree.label >> onEnter) startingState) Nothing updatedInit

                            else
                                init
                                    -- Kick off the state machine
                                    |> moveTowardsTarget config Nothing
                       )

            else
                Err <| "The starting state '" ++ startingStateName ++ "' is not a child of '" ++ branchStateConfig.name ++ "'"

        otherwise ->
            Err "(Impossible) the starting state was a leaf"


{-| Step through the statechart in response to an incoming message. This can fail in the case of non-existant state names.
-}
step : Config msg model -> msg -> ( model, Cmd msg ) -> Result String ( model, Cmd msg )
step ({ statechart, update, getStateModel, updateStateModel } as config) msg (( model, cmd ) as init) =
    findState ((getStateModel >> .currentStateName) model) statechart
        |> Result.andThen
            (\currentState ->
                let
                    -- Find the next state using the transition functions, if there is one
                    maybeTargetState : Maybe (Zipper msg model)
                    maybeTargetState =
                        getTransitionState msg model statechart currentState

                    isSelfTransition : Bool
                    isSelfTransition =
                        maybeTargetState == Just currentState
                in
                init
                    |> (\i ->
                            if isSelfTransition then
                                moveToParent config msg i

                            else
                                Ok i
                       )
                    -- Update the model with the new targetState
                    |> Result.map (Update.updateModel (updateStateModel (\m -> { m | targetStateName = maybeTargetState |> Maybe.withDefault currentState |> (label >> name) })))
                    -- Move to the target
                    |> Result.andThen (moveTowardsTarget config (Just msg))
            )


{-| Move one node towards the target updating the model and running any actions
-}
moveTowardsTarget : Config msg model -> Maybe msg -> ( model, Cmd msg ) -> Result String ( model, Cmd msg )
moveTowardsTarget ({ statechart, update, getStateModel, updateStateModel } as config) maybeMsg (( model, cmd ) as init) =
    findState ((getStateModel >> .currentStateName) model) statechart
        |> Result.andThen
            (\currentState ->
                let
                    chooseNextState : Zipper msg model -> Result String (Maybe (Zipper msg model))
                    chooseNextState targetState =
                        if currentState == targetState then
                            if List.isEmpty (children currentState) then
                                -- If we have no children we are done and there is no next state
                                Ok Nothing

                            else
                                -- If this is a compound state then we need to go to its starting state (as we can't end in a compound state)
                                targetState
                                    -- Get the starting state
                                    |> (\state ->
                                            case label state of
                                                BranchStateConfig branchStateConfig ->
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
                                                                |> Maybe.andThen
                                                                    (\transitionStateName ->
                                                                        if transitionStateName == branchStateConfig.name then
                                                                            Nothing

                                                                        else
                                                                            Just transitionStateName
                                                                    )
                                                                -- then try history
                                                                |> Maybe.or
                                                                    (if branchStateConfig.hasHistory then
                                                                        (getStateModel >> .history) model
                                                                            |> Dict.get branchStateConfig.name

                                                                     else
                                                                        Nothing
                                                                    )
                                                                -- finally fall back to the default starting states
                                                                |> Maybe.withDefault ((Tree.label >> name) branchStateConfig.startingState)
                                                    in
                                                    findState stateNameToStartIn statechart
                                                        |> Result.map Just

                                                LeafStateConfig _ ->
                                                    Err "The builder functions shouldn't ever allow this to occur"
                                       )
                            -- TODO: check that the starting state isn't the same as the targetState (as this would cause an infinite loop)

                        else
                            Ok (Just targetState)

                    calculateActionsAndStates : Zipper msg model -> Maybe (ActionsAndStates msg model)
                    calculateActionsAndStates targetState =
                        if contains (tree targetState) (tree currentState) then
                            -- We need to move one step down the tree (towards the target) and run the target's onEnter
                            let
                                writeHistoryAction : Zipper msg model -> Zipper msg model -> Action msg model
                                writeHistoryAction state compoundState =
                                    \_ modelToUpdate ->
                                        ( updateStateModel (\m -> { m | history = Dict.insert ((label >> name) compoundState) ((label >> name) state) m.history }) modelToUpdate
                                        , []
                                        )
                            in
                            firstChild currentState
                                |> Maybe.andThen (findSiblingContainingChild targetState)
                                |> Maybe.map
                                    (\state ->
                                        { beforeAction = noAction
                                        , nextState = state
                                        , targetState = targetState
                                        , afterAction =
                                            sequenceActions
                                                [ writeHistoryAction state currentState
                                                , (label >> onEnter) state
                                                ]
                                        }
                                    )

                        else
                            -- We need to move one step up the tree and run the current node's onExit
                            parent currentState
                                |> Maybe.map
                                    (\state ->
                                        { beforeAction =
                                            sequenceActions
                                                [ (label >> onExit) currentState
                                                ]
                                        , nextState = state
                                        , targetState = targetState
                                        , afterAction = noAction
                                        }
                                    )

                    performActionsAndState : Maybe (ActionsAndStates msg model) -> Result String ( model, Cmd msg )
                    performActionsAndState actionsAndStates =
                        case actionsAndStates of
                            Just { beforeAction, nextState, targetState, afterAction } ->
                                init
                                    -- Apply the before action (this will be an onExit if we are moving up the tree)
                                    |> applyAction update beforeAction maybeMsg
                                    -- Log the transition we are about to do
                                    |> (\x ->
                                            x
                                                -- Update the model with the new nextState and targetState
                                                |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = (label >> name) nextState, targetStateName = (label >> name) targetState }))
                                                -- Apply the after action (this will be an onEnter if we are moving down the tree)
                                                |> applyAction update afterAction maybeMsg
                                                -- Continue moving towards the target by recursing back into this function
                                                |> moveTowardsTarget config maybeMsg
                                       )

                            Nothing ->
                                Ok init
                in
                findState ((getStateModel >> .targetStateName) model) statechart
                    |> Result.andThen chooseNextState
                    |> Result.map (Maybe.andThen calculateActionsAndStates)
                    |> Result.andThen performActionsAndState
            )


{-| Move one node up the hierarchy updating the model and running any exit action (this is used in self-transitions)
-}
moveToParent : Config msg model -> msg -> ( model, Cmd msg ) -> Result String ( model, Cmd msg )
moveToParent ({ statechart, update, getStateModel, updateStateModel } as config) msg (( model, cmd ) as init) =
    findState ((getStateModel >> .currentStateName) model) statechart
        |> Result.map
            (\currentState ->
                let
                    result =
                        parent currentState
                            |> Maybe.map
                                (\state ->
                                    { beforeAction =
                                        sequenceActions
                                            [ (label >> onExit) currentState
                                            ]
                                    , nextState = state
                                    }
                                )
                in
                case result of
                    Just { beforeAction, nextState } ->
                        init
                            -- Apply the before action (this will be an onExit if we are moving up the tree)
                            |> applyAction update beforeAction (Just msg)
                            -- Update the model with the new nextState and targetState
                            |> Update.updateModel (updateStateModel (\m -> { m | currentStateName = (label >> name) nextState }))

                    Nothing ->
                        init
            )


{-| -}
findState : String -> Statechart msg model -> Result String (Zipper msg model)
findState nameToFind statechart =
    statechart
        |> fromTree
        |> findFromRoot (\stateConfig -> name stateConfig == nameToFind)
        |> Result.fromMaybe ("Attempt to find non-existent state " ++ nameToFind ++ ".  Check your statechart and transitions!")


{-| Helper function to say whether or not a parent contains a child
-}
contains : Statechart msg model -> Statechart msg model -> Bool
contains child parent =
    if (Tree.label >> name) child == (Tree.label >> name) parent then
        True

    else
        case Tree.children parent of
            [] ->
                False

            xs ->
                List.any (contains child) xs


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
    case (label >> transition) state msg model of
        Just stateName ->
            Result.toMaybe <| findState stateName statechart

        Nothing ->
            state
                |> parent
                |> Maybe.andThen (getTransitionState msg model statechart)


{-| Apply an action to a model/command tuple
-}
applyAction : (msg -> model -> ( model, Cmd msg )) -> Action msg model -> Maybe msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
applyAction update action maybeMsg ( model, cmd ) =
    let
        ( newModel, msgs ) =
            action maybeMsg model
    in
    ( newModel
    , cmd
    )
        |> Update.sequence update msgs


{-| Compose two actions together, one after another
-}
composeActions : Action msg model -> Action msg model -> Action msg model
composeActions g f =
    \msg model ->
        let
            ( fModel, fMsgs ) =
                f msg model

            ( gModel, gMsgs ) =
                g msg fModel
        in
        ( gModel, fMsgs ++ gMsgs )


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
        BranchStateConfig branchStateConfig ->
            branchStateConfig.name

        LeafStateConfig leafStateConfig ->
            leafStateConfig.name


{-| Get the state's onEnter from the config
-}
onEnter : StateConfig msg model -> Action msg model
onEnter stateConfig =
    case stateConfig of
        BranchStateConfig branchStateConfig ->
            branchStateConfig.onEnter

        LeafStateConfig leafStateConfig ->
            leafStateConfig.onEnter


{-| Get the state's onExit from the config
-}
onExit : StateConfig msg model -> Action msg model
onExit stateConfig =
    case stateConfig of
        BranchStateConfig branchStateConfig ->
            branchStateConfig.onExit

        LeafStateConfig leafStateConfig ->
            leafStateConfig.onExit


{-| Get the state's transition from the config
-}
transition : StateConfig msg model -> msg -> model -> Maybe String
transition stateConfig =
    case stateConfig of
        BranchStateConfig branchStateConfig ->
            branchStateConfig.transition

        LeafStateConfig leafStateConfig ->
            leafStateConfig.transition
