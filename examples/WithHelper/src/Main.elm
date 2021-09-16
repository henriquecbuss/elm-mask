module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Mask


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { exampleInput : String
    }


init : Model
init =
    { exampleInput = "0,00" }


type Msg
    = EnteredExampleInput String


view : Model -> Html Msg
view model =
    let
        inputId =
            "example-input"
    in
    Html.div []
        [ Html.input
            [ Html.Attributes.id inputId
            , Html.Attributes.value model.exampleInput
            , Html.Events.onInput EnteredExampleInput
            ]
            []
        , Html.node "masked-input-helper"
            [ Html.Attributes.attribute "target-id" inputId
            , Html.Attributes.attribute "mask-type" "number"
            , Html.Attributes.attribute "decimal-separator" separators.decimalSeparator
            ]
            []
        ]


separators : { decimalSeparator : String, thousandsSeparator : String }
separators =
    { decimalSeparator = ",", thousandsSeparator = "." }


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredExampleInput val ->
            { model
                | exampleInput =
                    Mask.updateFloatString
                        (Mask.Precisely 2)
                        separators
                        { previousValue = model.exampleInput, newValue = val }
            }
