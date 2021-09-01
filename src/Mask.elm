module Mask exposing
    ( string, remove
    , DecimalDigits(..), float, floatString
    )

{-| Mask

@docs string, remove

@docs DecimalDigits, float, floatString

-}


{-| Apply a simple mask to a `String`. You can customize the mask template and
which `Char` to replace.

    string { mask = "+1 ### ### ####", replace = '#' } "1234567890"
    --> "+1 123 456 7890"

-}
string : { mask : String, replace : Char } -> String -> String
string { mask, replace } source =
    let
        forbiddenChars : List Char
        forbiddenChars =
            mask
                |> String.filter
                    (\maskChar ->
                        (maskChar /= replace)
                            && not (Char.isAlphaNum maskChar)
                    )
                |> String.toList
    in
    source
        |> String.filter (\x -> not <| List.member x forbiddenChars)
        |> String.foldl (stringHelp replace) { currentString = "", currentMask = mask }
        |> .currentString
        |> String.reverse


stringHelp : Char -> Char -> { currentString : String, currentMask : String } -> { currentString : String, currentMask : String }
stringHelp replaceChar sourceChar { currentString, currentMask } =
    case String.toList currentMask of
        [] ->
            { currentString = currentString, currentMask = currentMask }

        firstOfMask :: restOfMask ->
            if firstOfMask == replaceChar then
                { currentString = String.cons sourceChar currentString
                , currentMask = String.fromList restOfMask
                }

            else
                { currentString = String.cons firstOfMask currentString
                , currentMask = String.fromList restOfMask
                }
                    |> stringHelp replaceChar sourceChar


{-| Remove a mask applied by [`string`](#string). This is often needed if you
want to save just the value, and not the formatted/masked `String`.

    remove { mask = "+1 ### ### ####", replace = '#' } "+1 123 456 7890"
    --> "1234567890"

    remove { mask = "+1 ### ### ####", replace = '#' } "1234567890"
    -->"1234567890"

-}
remove : { mask : String, replace : Char } -> String -> String
remove { mask, replace } masked =
    masked
        |> String.foldl (removeHelp replace) { currentString = "", currentMask = mask }
        |> .currentString
        |> String.reverse


removeHelp : Char -> Char -> { currentString : String, currentMask : String } -> { currentString : String, currentMask : String }
removeHelp replaceChar maskedChar { currentString, currentMask } =
    case String.toList currentMask of
        [] ->
            { currentString = currentString
            , currentMask = currentMask
            }

        firstOfMask :: restOfMask ->
            if firstOfMask == replaceChar then
                { currentString = String.cons maskedChar currentString
                , currentMask = String.fromList restOfMask
                }

            else
                { currentString = currentString
                , currentMask = String.fromList restOfMask
                }


{-| When formatting with [`float`](#float) and [`floatString`](#floatString), it can be useful to either use
exactly an amount of decimal digits, or just to limit it to a certain amount.
-}
type DecimalDigits
    = Precisely Int
    | AtMost Int


{-| Mask a float to have a certain amount of [`DecimalDigits`](#DecimalDigits).

    float (Precisely 2) 123
    --> "123.00"

    float (Precisely 2) 123.4
    --> "123.40"

    float (Precisely 2) 123.4567
    --> "123.45"

    float (AtMost 2) 123
    --> "123"

    float (AtMost 2) 123.4
    --> "123.4"

    float (AtMost 2) 123.4567
    --> "123.45"

-}
float : DecimalDigits -> Float -> String
float decimalDigits value =
    case value |> String.fromFloat |> String.split "." of
        [] ->
            -- IMPOSSIBLE CASE
            String.fromFloat value

        [ noSeparator ] ->
            [ Just noSeparator, padFloat decimalDigits Nothing ]
                |> List.filterMap identity
                |> String.join "."

        beforeSeparator :: afterSeparator :: _ ->
            [ Just beforeSeparator, padFloat decimalDigits (Just afterSeparator) ]
                |> List.filterMap identity
                |> String.join "."


{-| Mask a float to have a certain amount of [`DecimalDigits`](#DecimalDigits),
but using a `String` as an input. This is useful for input fields, where the
`onInput` event returns a `String` that can be run through `floatString`. In
case the input `String` is not a valid `Float`, this function returns `Nothing`,
and an empty `String` is automatically converted to `0`.

    floatString (Precisely 2) "123.4"
    --> Just "123.40"

    floatString (Precisely 2) "12a.4"
    --> Nothing

    floatString (Precisely 2) ""
    --> Just "0.00"

Usually you'll want to use this function in your `update` like this:

    update msg model =
        case msg of
            EnteredPrice price ->
                { model
                    | price =
                        price
                            |> Mask.floatString (Mask.Precisely 2)
                            |> Maybe.withDefault model.price
                }

-}
floatString : DecimalDigits -> String -> Maybe String
floatString decimalDigits value =
    let
        needsZeroDigits =
            case decimalDigits of
                Precisely 0 ->
                    True

                AtMost 0 ->
                    True

                _ ->
                    False

        valueWithTrailingZero =
            if String.endsWith "." value && not needsZeroDigits then
                value ++ "0"

            else
                value
    in
    if String.isEmpty value then
        float decimalDigits 0
            |> Just

    else
        valueWithTrailingZero
            |> String.toFloat
            |> Maybe.map
                (\floatValue ->
                    let
                        formattedFloat =
                            float decimalDigits floatValue
                    in
                    case decimalDigits of
                        Precisely _ ->
                            formattedFloat

                        AtMost n ->
                            if n > 0 && String.endsWith "." value then
                                formattedFloat ++ "."

                            else
                                formattedFloat
                )


{-| Add the necessary trailing `0`s to a float
-}
padFloat : DecimalDigits -> Maybe String -> Maybe String
padFloat decimalDigits maybeAfterSeparator =
    let
        emptyStringToNothing : String -> Maybe String
        emptyStringToNothing str =
            if String.isEmpty str then
                Nothing

            else
                Just str
    in
    case decimalDigits of
        Precisely n ->
            case maybeAfterSeparator of
                Nothing ->
                    String.repeat n "0"
                        |> emptyStringToNothing

                Just afterSeparator ->
                    afterSeparator
                        ++ String.repeat (n - String.length afterSeparator) "0"
                        |> String.left n
                        |> emptyStringToNothing

        AtMost n ->
            maybeAfterSeparator
                |> Maybe.map (String.left n)
                |> Maybe.andThen emptyStringToNothing
