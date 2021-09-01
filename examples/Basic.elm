
module Basic exposing (..)

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
    { exampleInput1 : String
    , exampleFloatInput : Float
    , exampleFloatInput2 : String
    }


init : Model
init =
    { exampleInput1 = ""
    , exampleFloatInput = 0
    , exampleFloatInput2 = Mask.floatString (Mask.AtMost 2) "" |> Maybe.withDefault "0"
    }


type Msg
    = EnteredExampleInput1 String
    | EnteredExampleFloatInput String
    | EnteredExampleFloatInput2 String


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "min-height" "100vh"
        ]
        [ Html.input
            [ Html.Attributes.value model.exampleInput1
            , Html.Events.onInput EnteredExampleInput1
            , Html.Attributes.style "margin-bottom" "2rem"
            ]
            []
        , Html.input
            [ Html.Attributes.value model.exampleFloatInput
            , Html.Events.onInput EnteredExampleFloatInput
            , Html.Attributes.style "margin-bottom" "2rem"
            ]
            []
        , Html.input
            [ Html.Attributes.value model.exampleFloatInput2
            , Html.Events.onInput EnteredExampleFloatInput2
            ]
            []
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredExampleInput1 val ->
            { model | exampleInput1 = Mask.string mask val }

        EnteredExampleFloatInput val ->
            { model
                | exampleFloatInput =
                    val
                        |> String.toFloat
                        |> Maybe.withDefault model.exampleFloatInput
                        |> Mask.float (Mask.Precisely 2)
            }

        EnteredExampleFloatInput2 val ->
            { model
                | exampleFloatInput2 =
                    val
                        |> Mask.floatString (Mask.AtMost 2)
                        |> Maybe.withDefault model.exampleFloatInput2
            }


mask : { mask : String, replace : Char }
mask =
    { mask = "(##) #####-####", replace = '#' }
