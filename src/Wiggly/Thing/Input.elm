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
import Dict

-- MODEL

type SaveState = New | Persisted

type alias Input =
  {
    name : String
  , label : String
  , value : String
  , valid : Bool
  }

-- this is the model for the inputs, they are all strings and we will provide
-- validation against the strings. To generate a call to create a LineItem we will convert
-- to the correct types at that time
type alias Model =
  {
    id : Maybe Int
  , inputs : Dict.Dict String Input
  , state : SaveState
  }

init : Model
init =
  {
    id = Nothing
  , inputs = Dict.fromList (List.map (\x -> (x.name, x) )
    [
     { name = "name", label = "Name", value = "", valid = True }
    , { name = "accountId", label = "Account", value = "", valid = True }
    , { name = "flavour", label = "Flavour", value = "", valid = True }
    ])
  , state = New
  }

-- this is only being used because we need to have a default when getting elements from the dictionary.
-- if there was a better way of selecting something from the record by name that would be great....so we could say model.inputs.<thing>
-- guess we could just have specific, named inputs and then pass them in....then we need to be able to update the correct one
-- when we get into the update code....which begs the question how do you get a record element without
-- it being statically determined?
defaultInput : Input
defaultInput = { name = "ERROR", label = "ERROR", value = "ERRROR", valid = False }

updateInputValue : Dict.Dict String Input -> String -> String -> Dict.Dict String Input
updateInputValue inputs k v = let newValue = (\old -> Maybe.map (\x -> { x | value = v }) old)
                              in Dict.update k newValue inputs

-- UPDATE

type Action = NoOp
            | Save
            | Reset
            | SetInput String String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Save -> { model | id = Just 42, state = Persisted }

    Reset -> init

    SetInput k v -> { model | inputs = updateInputValue model.inputs k v }



generalInput : String -> Dict.Dict String Input -> (Address Action) -> List Html
generalInput inputName inputs address =
  let myInput = withDefault defaultInput <| Dict.get inputName inputs
  in
    [
     label [for myInput.name] [text myInput.label]
    , input
     [
      name myInput.name
     , type' "text"
     , value myInput.value
     , on "input" targetValue (\value -> Signal.message address (SetInput myInput.name value))
     ]
     []
    ]

-- VIEW
view : Address Action -> Model -> Html
view address model =
  div []
        [ div []
                [
                 fieldset []
                            (
                             [
                              legend [] [text "Thing"]
                             , label [for "id"] [text "ID"]
                             , text <| withDefault "N/A" (Maybe.map toString model.id)
                             , br [] []
                             ]
                             ++ (generalInput "name" model.inputs address)
                             ++ [ br [] [] ]
                             ++ (generalInput "flavour" model.inputs address)
                             ++ [ br [] [] ]
                             ++ (generalInput "accountId" model.inputs address)
                             ++ [
                              br [] []
                             , button [onClick address Save] [text "Save"]
                             , br [] []
                             , button [onClick address Reset] [text "Reset"]
                             , br [] []
                             ]
                             ++ (debugView <| Dict.values model.inputs)
                            )
                ]
        ]

debugView : List Input -> List Html
debugView inputs = let debugInput x = text ("N: " ++ x.name ++ " - V: " ++ x.value)
                       debugInputs xs = List.map debugInput xs
                   in List.intersperse (br [] []) <| debugInputs inputs
