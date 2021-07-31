module Main exposing (..)

import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Char



-- MAIN


main : Program InitFlags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { draft : String
  , words : List String
  , letters : List Char
  }

type alias InitFlags =
  { letters : String
  }

init : InitFlags -> ( Model, Cmd Msg )
init flags =
  ( { draft = "", words = [] , letters = String.toList flags.letters |> List.map Char.toUpper }
  , Cmd.none
  )



-- UPDATE


type Msg
  = DraftChanged String
  | Submit
  | WordChecked (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DraftChanged draft ->
      ( { model | draft = draft }
      , Cmd.none
      )

    Submit ->
      ( { model | draft = "" }
      , validateWord model.draft model.letters
      )

    WordChecked result ->
      case result of 
        Ok word ->
          ( { model | words = model.words ++ [word] }
          , Cmd.none
          )

        Err error ->
          Debug.log (Debug.toString error)
          ( model, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Word Finder" ]
    , if List.length model.words > 0 then h2 [] [ text "Found Words" ] else text ""
    , ul []
        (List.map (\word -> li [] [ text word ]) model.words)
    , h2 [] [ text "Letters" ]
    , ul []
        (List.map (\letter -> li [] [ text (String.fromChar letter) ]) model.letters)
    , input
        [ type_ "text"
        , onInput DraftChanged
        , on "keydown" (ifIsEnter Submit)
        , value model.draft
        ]
        []
    , button [ onClick Submit ] [ text "Submit" ]
    ]



-- DETECT ENTER

ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")

-- VALIDATE WORD

validateWord : String -> List Char -> Cmd Msg
validateWord word letters =
  if isWordLongEnough word && isWordMadeOfValidLetters word letters then 
    getWordFromDictonary word
  else
    Cmd.none

isWordLongEnough : String -> Bool
isWordLongEnough word =
  String.length word >= 3


isWordMadeOfValidLetters : String -> List Char -> Bool
isWordMadeOfValidLetters word letters =
  String.toUpper word |> String.all (\letter -> List.member letter letters)



-- CHECK DICTIONARY

getWordFromDictonary : String -> Cmd Msg
getWordFromDictonary word =
  Http.get 
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en_US/" ++ word 
    , expect = Http.expectJson WordChecked wordDecoder
    }

wordDecoder : D.Decoder String
wordDecoder =
  D.index 0 (D.field "word" D.string)





