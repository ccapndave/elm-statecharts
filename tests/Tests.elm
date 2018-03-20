module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Statecharts exposing (..)


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


onEnterStatechart : Statechart Msg StateModel
onEnterStatechart =
  let
    stopped = mkState
      { name = stateNames.stopped
      , onEnter = \m -> (m, [ StartStop ]) -- this stopwatch should start running automatically
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
    
    , describe "onEnter changing state"
      [ test "Starting state" <| \_ ->
        let
          (model, cmd) =
            empty ! []
              |> start (config onEnterStatechart)
        in
        Expect.equal model.currentStateName stateNames.running
      ]
    
    , describe "history"
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