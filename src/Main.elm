module Main exposing (..)

import Browser
import Html exposing (..)

-- Main

main : Program Int Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model = { currentTime : Int }

init : Int -> (Model, Cmd Msg)
init currentTime =
  ( {currentTime = currentTime}
  , Cmd.none
  )

-- Update

type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

-- View

view : Model -> Html Msg
view model =
  text (String.fromInt model.currentTime)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none