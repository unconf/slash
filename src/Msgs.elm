module Msgs exposing (..)

import Navigation exposing (Location)
import Window exposing (Size)

type Msg
    = OnLocationChange Location
    | OnTextChanged String
    | OnResize Size
