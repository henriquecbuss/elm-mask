module Mask exposing
    ( string, remove
    , DecimalDigits(..), float, floatString
    , defaultSeparators, removeFloat, updateFloatString
    )

{-| This library helps you mask `String`s and `Float`s


# Strings

@docs string, remove


# Floats

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

    float (Precisely 2) { decimalSeparator = ".", thousandsSeparator = "" } 1234
    --> "1234.00"

    float (Precisely 2) { decimalSeparator = ",", thousandsSeparator = " " } 1234.5
    --> "1 234,50"

    float (Precisely 2) { decimalSeparator = ",", thousandsSeparator = "" } 123.4567
    --> "123,45"

    float (AtMost 2) { decimalSeparator = ".", thousandsSeparator = "," } 123
    --> "123"

    float (AtMost 2) { decimalSeparator = ".", thousandsSeparator = "," } 123.4
    --> "123.4"

    float (AtMost 2) { decimalSeparator = ".", thousandsSeparator = "," } 123.4567
    --> "123.45"

-}
float : DecimalDigits -> { decimalSeparator : String, thousandsSeparator : String } -> Float -> String
float decimalDigits { decimalSeparator, thousandsSeparator } value =
    case value |> String.fromFloat |> String.split "." of
        [] ->
            -- IMPOSSIBLE CASE
            String.fromFloat value

        [ noSeparator ] ->
            [ Just (addThousandsSeparator thousandsSeparator noSeparator)
            , padFloat decimalDigits Nothing
            ]
                |> List.filterMap identity
                |> String.join decimalSeparator

        beforeSeparator :: afterSeparator :: _ ->
            [ Just (addThousandsSeparator thousandsSeparator beforeSeparator)
            , padFloat decimalDigits (Just afterSeparator)
            ]
                |> List.filterMap identity
                |> String.join decimalSeparator


{-| Mask a float to have a certain amount of [`DecimalDigits`](#DecimalDigits),
but using a `String` as an input. This is useful for input fields, where the
`onInput` event returns a `String` that can be run through `floatString`. In
case the input `String` is not a valid `Float`, this function returns `Nothing`,
and an empty `String` is automatically converted to `0`.

    floatString (Precisely 2) { decimalSeparator = ",", thousandsSeparator = " " } "1234.5"
    --> Just "1 234,50"

    floatString (Precisely 2) { decimalSeparator = ".", thousandsSeparator = "," } "12a.4"
    --> Nothing

    floatString (Precisely 2) { decimalSeparator = ",", thousandsSeparator = "." } ""
    --> Just "0,00"

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
floatString : DecimalDigits -> { decimalSeparator : String, thousandsSeparator : String } -> String -> Maybe String
floatString decimalDigits separators value =
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
        float decimalDigits separators 0
            |> Just

    else
        valueWithTrailingZero
            |> String.toFloat
            |> Maybe.map
                (\floatValue ->
                    let
                        formattedFloat =
                            float decimalDigits separators floatValue
                    in
                    case decimalDigits of
                        Precisely _ ->
                            formattedFloat

                        AtMost n ->
                            if n > 0 && String.endsWith "." value then
                                formattedFloat ++ separators.decimalSeparator

                            else
                                formattedFloat
                )


{-| A default value you can use for [`float`](#float) and [`floatString`](#floatString).
-}
defaultSeparators : { decimalSeparator : String, thousandsSeparator : String }
defaultSeparators =
    { decimalSeparator = ".", thousandsSeparator = "" }


{-| Remove a mask applied by [`float`](#float) or [`floatString`](#floatString)

    removeFloat { decimalSeparator = ",", thousandsSeparator = " " } "1 234,56"
    --> "1234.56"

-}
removeFloat : { decimalSeparator : String, thousandsSeparator : String } -> String -> String
removeFloat { decimalSeparator, thousandsSeparator } value =
    value
        |> String.replace thousandsSeparator ""
        |> String.replace decimalSeparator "."


{-| Update a masked value you store in your `Model`. This is useful to provide a
better UX when using masked input fields. For more details, check out
[this example on GitHub](https://github.com/NeoVier/elm-mask/tree/main/examples/WithHelper)
or in this [Ellie link](https://ellie-app.com/fjCBtqtVY8Ma1), which uses a
[custom element](https://guide.elm-lang.org/interop/custom_elements.html) to
make it even better!
-}
updateFloatString : DecimalDigits -> { decimalSeparator : String, thousandsSeparator : String } -> { previousValue : String, newValue : String } -> String
updateFloatString decimalDigits ({ decimalSeparator } as separators) { previousValue, newValue } =
    let
        decimalDigitsAmount =
            case decimalDigits of
                Precisely x ->
                    x

                AtMost x ->
                    x

        multiplyBy10 : String -> String
        multiplyBy10 value =
            case String.split "." value of
                [ beforeSeparator ] ->
                    if String.length beforeSeparator == 1 then
                        "0." ++ String.repeat (decimalDigitsAmount - 1) "0" ++ beforeSeparator

                    else
                        beforeSeparator

                [ beforeSeparator, afterSeparator ] ->
                    if String.length afterSeparator > decimalDigitsAmount then
                        [ beforeSeparator ++ String.left 1 afterSeparator
                        , String.dropLeft 1 afterSeparator
                        ]
                            |> String.join "."

                    else if String.length afterSeparator < decimalDigitsAmount then
                        [ String.dropRight 1 beforeSeparator
                        , String.right 1 beforeSeparator ++ afterSeparator
                        ]
                            |> String.join "."

                    else
                        value

                _ ->
                    value

        removeZeroes : String -> String
        removeZeroes value =
            case String.toFloat previousValue of
                Nothing ->
                    value

                Just floatValue ->
                    if floatValue == 0 then
                        case String.toFloat value of
                            Nothing ->
                                String.replace "0" "" value

                            Just currentFloatValue ->
                                if currentFloatValue >= 1 then
                                    String.replace "0" "" value

                                else
                                    value

                    else
                        value

        handleDeletingSeparator : String -> String
        handleDeletingSeparator value =
            if String.contains "." previousValue && not (String.contains "." newValue) then
                case String.split decimalSeparator value of
                    [ beforeSeparator, afterSeparator ] ->
                        if String.endsWith (String.repeat decimalDigitsAmount "0") beforeSeparator then
                            String.dropRight decimalDigitsAmount beforeSeparator ++ decimalSeparator ++ afterSeparator

                        else
                            beforeSeparator ++ decimalSeparator ++ afterSeparator

                    _ ->
                        -- IMPOSSIBLE CASE
                        value

            else
                value
    in
    newValue
        |> removeFloat separators
        |> multiplyBy10
        |> removeZeroes
        |> floatString decimalDigits separators
        |> Maybe.map handleDeletingSeparator
        |> Maybe.withDefault previousValue


{-| Adds a `separator` to a `value`. The `value` is the left side of a float
string (without the decimal digits)
-}
addThousandsSeparator : String -> String -> String
addThousandsSeparator separator value =
    value
        |> String.foldr
            (\currentChar { currentCount, currentGroups, currentGroup } ->
                if currentCount == 2 then
                    { currentCount = 0
                    , currentGroups = String.cons currentChar currentGroup :: currentGroups
                    , currentGroup = ""
                    }

                else
                    { currentCount = currentCount + 1
                    , currentGroups = currentGroups
                    , currentGroup = String.cons currentChar currentGroup
                    }
            )
            { currentCount = 0, currentGroups = [], currentGroup = "" }
        |> (\{ currentGroups, currentGroup } -> currentGroup :: currentGroups)
        |> List.filter (not << String.isEmpty)
        |> String.join separator


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
