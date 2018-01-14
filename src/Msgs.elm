module Msgs exposing (..)

import Navigation exposing (Location)
import Window exposing (Size)
import Dom

type Msg
    = OnLocationChange Location
    | OnTextChanged String
    | OnEnter
    | OnResize Size
    | OnRefocus
    | OnTaskResult (Result Dom.Error ())
