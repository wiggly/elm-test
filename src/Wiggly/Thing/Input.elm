module Wiggly.Thing.Input where

--
-- HTML Input test
--

import Html exposing (..)
import Html.Attributes exposing (key, class, for, type', name, value)
import Html.Events exposing (onClick, targetValue, on, onBlur)
import Maybe exposing (withDefault)
import String exposing (toInt)
import Signal exposing (Address)

-- MODEL
type SaveState = New | Persisted

-- this is the model for the inputs, they are all strings and we will provide
-- validation against the strings. To generate a call to create a LineItem we will convert
-- to the correct types at that time
type alias Model =
  {
    id : Maybe Int
  , name : String
  , accountId : String
  , state : SaveState
  }

init : Model
init =
  {
    id = Nothing
  , name = ""
  , accountId = ""
  , state = New
  }

-- UPDATE

type Action = NoOp
            | Save
            | Reset
            | SetName String
            | SetAccountId String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Save -> { model | id = Just 42, state = Persisted }

    Reset -> init

    SetName name' -> { model | name = name' }

    SetAccountId accountId' -> { model | accountId = accountId' }


-- VIEW
view : Address Action -> Model -> Html
view address model =
  div []
        [ div []
                 [
                  fieldset []
                             [
                              legend [] [text "Thing"]
                             , label [for "id"] [text "ID"]
                             , text <| withDefault "N/A" (Maybe.map toString model.id)
                             , br [] []
                             , label [for "name"] [text "Name"]
                             , input
                              [
                               name "name"
                              , type' "text"
                              , value model.name
                              , on "input" targetValue (\value -> Signal.message address (SetName value))
                                     ]
                              []
                             , br [] []
                             , label [for "accountId"] [text "Account"]
                             , input
                              [
                               name "accountId"
                              , type' "text"
                              , value model.accountId
                              , on "input" targetValue (\value -> Signal.message address (SetAccountId value))
                              ]
                              []
                             , br [] []
                             , button [onClick address Save] [text "Save"]
                             , br [] []
                             , button [onClick address Reset] [text "Reset"]
                             ]
                 , text ("NAME: " ++ model.name) , br [] []
                 , text ("  AID: " ++ model.accountId) , br [] []
                 , text ("  S: " ++ toString model.state) , br [] []
                 ]
        ]
