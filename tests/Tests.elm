module Tests exposing (Msg(..), config, flatStatechart, hierarchicalStatechart, historyStatechart, stateNames, suite, veryHierarchicalStatechart)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Statechart exposing (..)
import Test exposing (..)


type Msg
    = StartStop
    | Reset
      -- These are for testing history
    | Next
    | On
    | Off


stateNames =
    { active = "ACTIVE"
    , stopped = "STOPPED"
    , running = "RUNNING"

    -- These are for testing history
    , a = "A"
    , b = "B"
    , c = "C"
    , on = "ON"
    , off = "OFF"
    , top = "TOP"
    , parent = "PARENT"
    , child1 = "CHILD1"
    , child2 = "CHILD2"
    }


flatStatechart : Statechart Msg StateModel
flatStatechart =
    let
        stopped =
            mkState
                { name = stateNames.stopped
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            StartStop ->
                                Just stateNames.running

                            otherwise ->
                                Nothing
                }

        running =
            mkState
                { name = stateNames.running
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            StartStop ->
                                Just stateNames.stopped

                            otherwise ->
                                Nothing
                }
    in
    mkStatechart stopped [ stopped, running ]


hierarchicalStatechart : Statechart Msg StateModel
hierarchicalStatechart =
    let
        active =
            let
                stopped =
                    mkState
                        { name = stateNames.stopped
                        , onEnter = noAction
                        , onExit = noAction
                        , transition =
                            \msg _ ->
                                case msg of
                                    StartStop ->
                                        Just stateNames.running

                                    otherwise ->
                                        Nothing
                        }

                running =
                    mkState
                        { name = stateNames.running
                        , onEnter = noAction
                        , onExit = noAction
                        , transition =
                            \msg _ ->
                                case msg of
                                    StartStop ->
                                        Just stateNames.stopped

                                    otherwise ->
                                        Nothing
                        }
            in
            mkCompoundState
                { name = stateNames.active
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            Reset ->
                                Just stateNames.active

                            otherwise ->
                                Nothing
                , startingState = stopped
                , hasHistory = False
                }
                [ stopped, running ]
    in
    mkStatechart active [ active ]


historyStatechart : Statechart Msg StateModel
historyStatechart =
    let
        a =
            mkState
                { name = stateNames.a
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            Next ->
                                Just stateNames.b

                            otherwise ->
                                Nothing
                }

        b =
            mkState
                { name = stateNames.b
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            Next ->
                                Just stateNames.c

                            otherwise ->
                                Nothing
                }

        c =
            mkState
                { name = stateNames.c
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            Next ->
                                Just stateNames.a

                            otherwise ->
                                Nothing
                }

        on =
            mkCompoundState
                { name = stateNames.on
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            Off ->
                                Just stateNames.off

                            otherwise ->
                                Nothing
                , startingState = a
                , hasHistory = True
                }
                [ a, b, c ]

        off =
            mkState
                { name = stateNames.off
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            On ->
                                Just stateNames.on

                            otherwise ->
                                Nothing
                }
    in
    mkStatechart off [ on, off ]


veryHierarchicalStatechart : Statechart Msg StateModel
veryHierarchicalStatechart =
    let
        a =
            let
                b =
                    let
                        c =
                            mkState
                                { name = stateNames.c
                                , onEnter = noAction
                                , onExit = noAction
                                , transition = \_ _ -> Nothing
                                }
                    in
                    mkCompoundState
                        { name = stateNames.b
                        , onEnter = noAction
                        , onExit = noAction
                        , startingState = c
                        , transition = \_ _ -> Nothing
                        , hasHistory = False
                        }
                        [ c ]
            in
            mkCompoundState
                { name = stateNames.a
                , onEnter = noAction
                , onExit = noAction
                , startingState = b
                , transition = \_ _ -> Nothing
                , hasHistory = False
                }
                [ b ]
    in
    mkStatechart a [ a ]


config statechart =
    { statechart = statechart
    , update =
        \msg model ->
            case step (config statechart) msg ( model, Cmd.none ) of
                Ok init ->
                    init

                Err err ->
                    Debug.log "Update error" err |> (\_ -> ( model, Cmd.none ))
    , getStateModel = identity
    , updateStateModel = identity
    }


addOneAction msg model =
    ( { model | a = model.a + 1 }, [] )


actionStatechart : Statechart Msg { a : Int, stateModel : StateModel }
actionStatechart =
    let
        stopped =
            mkState
                { name = stateNames.stopped
                , onEnter = \msg model -> ( model, [ StartStop ] ) -- this stopwatch should start running automatically
                , onExit = addOneAction
                , transition =
                    \msg _ ->
                        case msg of
                            StartStop ->
                                Just stateNames.running

                            otherwise ->
                                Nothing
                }

        running =
            mkState
                { name = stateNames.running
                , onEnter = addOneAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            StartStop ->
                                Just stateNames.stopped

                            otherwise ->
                                Nothing
                }
    in
    mkStatechart stopped [ stopped, running ]


actionConfig =
    { statechart = actionStatechart
    , update =
        \msg model ->
            case step actionConfig msg ( model, Cmd.none ) of
                Ok init ->
                    init

                Err err ->
                    Debug.log "Update error" err |> (\_ -> ( model, Cmd.none ))
    , getStateModel = .stateModel
    , updateStateModel = \f m -> { m | stateModel = f m.stateModel }
    }


localTransitionStatechart : Statechart Msg StateModel
localTransitionStatechart =
    let
        top =
            mkState
                { name = stateNames.top
                , onEnter = noAction
                , onExit = noAction
                , transition =
                    \msg _ ->
                        case msg of
                            Next ->
                                Just stateNames.parent

                            otherwise ->
                                Nothing
                }

        parent =
            let
                child1 =
                    mkState
                        { name = stateNames.child1
                        , onEnter = noAction
                        , onExit = noAction
                        , transition = \_ _ -> Nothing
                        }

                child2 =
                    mkState
                        { name = stateNames.child2
                        , onEnter = noAction
                        , onExit = noAction
                        , transition = \_ _ -> Nothing
                        }
            in
            mkCompoundState
                { name = stateNames.parent
                , onEnter = noAction
                , onExit = noAction
                , startingState = child1
                , transition =
                    \msg _ ->
                        case msg of
                            Next ->
                                Just stateNames.child2

                            otherwise ->
                                Nothing
                , hasHistory = False
                }
                [ child1
                , child2
                ]
    in
    mkStatechart top [ top, parent ]


localTransitionConfig =
    { statechart = localTransitionStatechart
    , update =
        \msg model ->
            case step localTransitionConfig msg ( model, Cmd.none ) of
                Ok init ->
                    init

                Err err ->
                    Debug.log "Update error" err |> (\_ -> ( model, Cmd.none ))
    , getStateModel = identity
    , updateStateModel = identity
    }


resultEqual : (model -> Expectation) -> Result String ( model, Cmd msg ) -> Expectation
resultEqual p result =
    case result of
        Ok ( model, cmd ) ->
            p model

        Err err ->
            Expect.fail err


suite : Test
suite =
    describe "Statechart"
        [ describe "Flat"
            [ test "Starting state" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config flatStatechart)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.stopped)
            , test "Step" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config flatStatechart)
                        |> Result.andThen (step (config flatStatechart) StartStop)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.running)
            , test "Multi-steps" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config flatStatechart)
                        |> Result.andThen (step (config flatStatechart) StartStop)
                        |> Result.andThen (step (config flatStatechart) StartStop)
                        |> Result.andThen (step (config flatStatechart) StartStop)
                        |> Result.andThen (step (config flatStatechart) StartStop)
                        |> Result.andThen (step (config flatStatechart) StartStop)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.running)
            ]
        , describe "Hierarchical"
            [ test "Starting state" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config hierarchicalStatechart)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.stopped)
            , test "Step 1" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config hierarchicalStatechart)
                        |> Result.andThen (step (config hierarchicalStatechart) StartStop)
                        |> Result.andThen (step (config hierarchicalStatechart) Reset)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.stopped)
            , test "Step 2" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config hierarchicalStatechart)
                        |> Result.andThen (step (config hierarchicalStatechart) StartStop)
                        |> Result.andThen (step (config hierarchicalStatechart) StartStop)
                        |> Result.andThen (step (config hierarchicalStatechart) Reset)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.stopped)
            ]
        , describe "Very hierarchical"
            [ test "Starting state" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config veryHierarchicalStatechart)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.c)
            ]
        , describe "Actions" <|
            [ test "onEnter" <|
                \_ ->
                    ( { a = 0, stateModel = empty }, Cmd.none )
                        |> start actionConfig
                        |> resultEqual (\model -> Expect.equal model.stateModel.currentStateName stateNames.running)
            , test "onExit" <|
                \_ ->
                    ( { a = 1, stateModel = empty }, Cmd.none )
                        |> start actionConfig
                        |> resultEqual (\model -> Expect.equal model.a 3)
            ]
        , describe "Local transitions"
            [ test "Starting state" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start localTransitionConfig
                        |> Result.andThen (step localTransitionConfig Next)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.child2)
            ]
        , describe "History"
            [ test "Starting state" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config historyStatechart)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.off)
            , test "Step 1" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config historyStatechart)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.a)
            , test "Step 2" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config historyStatechart)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.c)
            , test "Step 3" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config historyStatechart)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Off)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.off)
            , test "History 1" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config historyStatechart)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Off)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.c)
            , test "History 2" <|
                \_ ->
                    ( empty, Cmd.none )
                        |> start (config historyStatechart)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Off)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> Result.andThen (step (config historyStatechart) Next)
                        |> Result.andThen (step (config historyStatechart) Off)
                        |> Result.andThen (step (config historyStatechart) On)
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.a)
            , test "Starting history" <|
                \_ ->
                    let
                        startHistoryStatechart : Statechart Msg StateModel
                        startHistoryStatechart =
                            let
                                top =
                                    let
                                        on =
                                            let
                                                a =
                                                    mkState
                                                        { name = stateNames.a
                                                        , onEnter = noAction
                                                        , onExit = noAction
                                                        , transition =
                                                            \msg _ ->
                                                                case msg of
                                                                    Next ->
                                                                        Just stateNames.b

                                                                    otherwise ->
                                                                        Nothing
                                                        }

                                                b =
                                                    mkState
                                                        { name = stateNames.b
                                                        , onEnter = noAction
                                                        , onExit = noAction
                                                        , transition =
                                                            \msg _ ->
                                                                case msg of
                                                                    Next ->
                                                                        Just stateNames.c

                                                                    otherwise ->
                                                                        Nothing
                                                        }

                                                c =
                                                    mkState
                                                        { name = stateNames.c
                                                        , onEnter = noAction
                                                        , onExit = noAction
                                                        , transition =
                                                            \msg _ ->
                                                                case msg of
                                                                    Next ->
                                                                        Just stateNames.a

                                                                    otherwise ->
                                                                        Nothing
                                                        }
                                            in
                                            mkCompoundState
                                                { name = stateNames.on
                                                , onEnter = noAction
                                                , onExit = noAction
                                                , transition =
                                                    \msg _ ->
                                                        case msg of
                                                            Off ->
                                                                Just stateNames.off

                                                            otherwise ->
                                                                Nothing
                                                , startingState = a
                                                , hasHistory = True
                                                }
                                                [ a, b, c ]

                                        off =
                                            mkState
                                                { name = stateNames.off
                                                , onEnter = noAction
                                                , onExit = noAction
                                                , transition =
                                                    \msg _ ->
                                                        case msg of
                                                            On ->
                                                                Just stateNames.on

                                                            otherwise ->
                                                                Nothing
                                                }
                                    in
                                    mkCompoundState
                                        { name = stateNames.top
                                        , onEnter = noAction
                                        , onExit = noAction
                                        , transition =
                                            \msg _ ->
                                                case msg of
                                                    Off ->
                                                        Just stateNames.off

                                                    otherwise ->
                                                        Nothing
                                        , startingState = off
                                        , hasHistory = True
                                        }
                                        [ on, off ]
                            in
                            mkStatechart top [ top ]
                    in
                    ( empty, Cmd.none )
                        |> start (config startHistoryStatechart)
                        |> Result.andThen (step (config startHistoryStatechart) On)
                        |> Result.andThen (step (config startHistoryStatechart) Next)
                        |> Result.andThen (step (config startHistoryStatechart) Next)
                        -- This should follow the history and end up back in the same state
                        |> Result.andThen (start (config startHistoryStatechart))
                        |> resultEqual (\model -> Expect.equal model.currentStateName stateNames.c)
            ]
        ]
