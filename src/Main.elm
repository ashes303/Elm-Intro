module Main exposing (..)

import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Random



-- MAIN


main : Program () Model Msg
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

init : () -> ( Model, Cmd Msg )
init _ =
  ( { draft = "", words = [] , letters = [] }
  , getLetterCode
  )



-- UPDATE


type Msg
  = DraftChanged String
  | Submit
  | WordChecked (Result Http.Error String)
  | NewLetterGenerated Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    DraftChanged draft ->
      ( { model | draft = draft }
      , Cmd.none
      )

    Submit ->
      ( { model | draft = "" }
      , validateWord model
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

    NewLetterGenerated newLetterCode ->
      let
        newLetter = Char.fromCode newLetterCode
      in
        if List.member newLetter model.letters then 
          (model, getLetterCode)
        else
          ( { model | letters = model.letters ++ [newLetter] }
          , if List.length model.letters < 7 then getLetterCode else Cmd.none)



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


-- GENERATE LETTERS

letterCodeGen : Random.Generator Int
letterCodeGen =
  Random.int 65 90
  
getLetterCode : Cmd Msg
getLetterCode =
  Random.generate NewLetterGenerated letterCodeGen


-- DETECT ENTER

ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")

-- VALIDATE WORD

validateWord : Model -> Cmd Msg
validateWord model =
  if isWordLongEnough model.draft 
  && isWordMadeOfValidLetters model.draft model.letters 
  && isNewlyFoundWord model.draft model.words then 
    getWordFromDictonary model.draft
  else
    Cmd.none

isWordLongEnough : String -> Bool
isWordLongEnough word =
  String.length word >= 3


isWordMadeOfValidLetters : String -> List Char -> Bool
isWordMadeOfValidLetters word letters =
  String.toUpper word |> String.all (\letter -> List.member letter letters)


isNewlyFoundWord : String -> List String -> Bool
isNewlyFoundWord word foundWords =
  List.member word foundWords |> not

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





