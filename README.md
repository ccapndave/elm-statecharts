# elm-statecharts

This is an Elm implementation of Harel Statecharts (the original paper is at
http://www.inf.ed.ac.uk/teaching/courses/seoc/2005_2006/resources/statecharts.pdf).

As well as all the normal features expected from state machines, it supports the
following more advanced features:

- hierarchical (compound) states
- history
- self-transitions

It is also designed to hook directly into the Elm Architecture's `update` function
and responds to `Msg`s in order to trigger transitions.
