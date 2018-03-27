module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Statechart exposing (..)
import Dict


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
  }


flatStatechart : Statechart Msg StateModel
flatStatechart =
  let
    stopped = mkState
      { name = stateNames.stopped
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        StartStop -> Just stateNames.running
        otherwise -> Nothing
      }

    running = mkState
      { name = stateNames.running
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        StartStop -> Just stateNames.stopped
        otherwise -> Nothing
      } 
  in
  mkStatechart stateNames.stopped [ stopped, running ]


hierarchicalStatechart : Statechart Msg StateModel
hierarchicalStatechart =
  let
    stopped = mkState
      { name = stateNames.stopped
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        StartStop -> Just stateNames.running
        otherwise -> Nothing
      }

    running = mkState
      { name = stateNames.running
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        StartStop -> Just stateNames.stopped
        otherwise -> Nothing
      } 

    active = mkCompoundState
      { name = stateNames.active
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        Reset -> Just stateNames.active
        otherwise -> Nothing
      , startingState = stateNames.stopped
      , hasHistory = False
      }
      [ stopped, running ]
  in  
  mkStatechart stateNames.active [ active ]


historyStatechart : Statechart Msg StateModel
historyStatechart =
  let
    a = mkState
      { name = stateNames.a
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        Next -> Just stateNames.b
        otherwise -> Nothing
      }

    b = mkState
      { name = stateNames.b
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        Next -> Just stateNames.c
        otherwise -> Nothing
      }

    c = mkState
      { name = stateNames.c
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        Next -> Just stateNames.a
        otherwise -> Nothing
      }
    
    on = mkCompoundState
      { name = stateNames.on
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        Off -> Just stateNames.off
        otherwise -> Nothing
      , startingState = stateNames.a
      , hasHistory = True
      }
      [ a, b, c ]
    
    off = mkState
      { name = stateNames.off
      , onEnter = noAction
      , onExit = noAction
      , transition = \msg _ -> case msg of
        On -> Just stateNames.on
        otherwise -> Nothing
      }
  in  
  mkStatechart stateNames.off [ on, off ]


veryHierarchicalStatechart : Statechart Msg StateModel
veryHierarchicalStatechart =
  let
    a =
      let
        b =
          let
            c = mkState
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
            , startingState = stateNames.c
            , transition = \_ _ -> Nothing
            , hasHistory = False
            }
            [ c ]
      in
      mkCompoundState
        { name = stateNames.a
        , onEnter = noAction
        , onExit = noAction
        , startingState = stateNames.b
        , transition = \_ _ -> Nothing
        , hasHistory = False
        }
        [ b ]
  in
  mkStatechart stateNames.a [a]


config statechart =
  { statechart = statechart
  , update = \msg model -> step (config statechart) msg (model, Cmd.none)
  , getStateModel = identity
  , updateStateModel = identity
  }


suite : Test
suite =
  describe "Statechart"
    [ describe "Flat"
      [ test "Starting state" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config flatStatechart)
        in
        Expect.equal model.currentStateName stateNames.stopped
      
      , test "Step" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config flatStatechart)
              |> step (config flatStatechart) StartStop
        in
        Expect.equal model.currentStateName stateNames.running
      
      , test "Multi-steps" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config flatStatechart)
              |> step (config flatStatechart) StartStop
              |> step (config flatStatechart) StartStop
              |> step (config flatStatechart) StartStop
              |> step (config flatStatechart) StartStop
              |> step (config flatStatechart) StartStop
        in
        Expect.equal model.currentStateName stateNames.running
      ]
    
    , describe "Hierarchical"
      [ test "Starting state" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config hierarchicalStatechart)
        in
        Expect.equal model.currentStateName stateNames.stopped
      
      , test "Step 1" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config hierarchicalStatechart)
              |> step (config hierarchicalStatechart) StartStop
              |> step (config hierarchicalStatechart) Reset
        in
        Expect.equal model.currentStateName stateNames.stopped
      
      , test "Step 2" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config hierarchicalStatechart)
              |> step (config hierarchicalStatechart) StartStop
              |> step (config hierarchicalStatechart) StartStop 
              |> step (config hierarchicalStatechart) Reset
        in
        Expect.equal model.currentStateName stateNames.stopped
      ]

    , only <| describe "Very hierarchical"
        [ test "Starting state" <| \_ ->
          let
            (model, cmd) =
              empty ! []
                |> start (config veryHierarchicalStatechart)
          in
          Expect.equal model.currentStateName stateNames.c
        ]

    , describe "Actions" <|
      let
        addOneAction model =
          ({ model | a = model.a + 1 }, [])

        actionStatechart : Statechart Msg { a : Int, stateModel : StateModel }
        actionStatechart =
          let
            stopped = mkState
              { name = stateNames.stopped
              , onEnter = \m -> (m, [ StartStop ]) -- this stopwatch should start running automatically
              , onExit = addOneAction
              , transition = \msg _ -> case msg of
                StartStop -> Just stateNames.running
                otherwise -> Nothing
              }

            running = mkState
              { name = stateNames.running
              , onEnter = addOneAction
              , onExit = noAction
              , transition = \msg _ -> case msg of
                StartStop -> Just stateNames.stopped
                otherwise -> Nothing
              }
          in  
          mkStatechart stateNames.stopped [ stopped, running ]

        actionConfig =
          { statechart = actionStatechart
          , update = \msg model -> step actionConfig msg (model, Cmd.none)
          , getStateModel = .stateModel
          , updateStateModel = \f m -> { m | stateModel = f m.stateModel }
          }
      in
      [ test "onEnter" <| \_ ->
        let
          (model, cmd) =
            { a = 0, stateModel = empty } ! []
              |> start actionConfig
        in
        Expect.equal model.stateModel.currentStateName stateNames.running
      
      , test "onExit" <| \_ ->
        let
          (model, cmd) =
            { a = 1, stateModel = empty } ! []
              |> start actionConfig
        in
        Expect.equal model.a 3
      
      , test "Start onEnter action" <| \_ ->
        let
          -- Make sure that starting in a state causes its onEnter to be fired
          (model, cmd) =
            { a = 0, stateModel = { currentStateName = stateNames.running, targetStateName = stateNames.running, history = Dict.empty } } ! []
              |> start actionConfig
        in
        Expect.equal model.a 1
      ]
    
    , describe "History"
      [ test "Starting state" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config historyStatechart)
        in
        Expect.equal model.currentStateName stateNames.off
      
      , test "Step 1" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config historyStatechart)
              |> step (config historyStatechart) On
        in
        Expect.equal model.currentStateName stateNames.a
      
      , test "Step 2" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config historyStatechart)
              |> step (config historyStatechart) On
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Next
        in
        Expect.equal model.currentStateName stateNames.c
      
      , test "Step 3" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config historyStatechart)
              |> step (config historyStatechart) On
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Off
        in
        Expect.equal model.currentStateName stateNames.off
      
      , test "History 1" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config historyStatechart)
              |> step (config historyStatechart) On
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Off
              |> step (config historyStatechart) On
        in
        Expect.equal model.currentStateName stateNames.c
      
      , test "History 2" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config historyStatechart)
              |> step (config historyStatechart) On
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Off
              |> step (config historyStatechart) On
              |> step (config historyStatechart) Next
              |> step (config historyStatechart) Off
              |> step (config historyStatechart) On
        in
        Expect.equal model.currentStateName stateNames.a
      ]
    ]