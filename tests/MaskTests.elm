module MaskTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Mask
import Random
import Random.Char
import Random.Extra
import Shrink exposing (Shrinker)
import Test exposing (..)


suite : Test
suite =
    describe "Mask Tests"
        [ fuzz2 stringMaskFuzzer Fuzz.string "Masking a string and unmasking it returns the original string" <|
            \fuzzMask fuzzString ->
                fuzzString
                    |> Mask.string fuzzMask
                    |> Mask.remove fuzzMask
                    |> Expect.equal
                        (String.filter ((==) fuzzMask.replace) fuzzMask.mask
                            |> String.length
                            |> (\maxLength -> String.left maxLength fuzzString)
                        )
        , test "Correctly masks brazilian phone number" <|
            \() ->
                "47912341234"
                    |> Mask.string { mask = "(##) #####-####", replace = '#' }
                    |> Expect.equal "(47) 91234-1234"
        , test "Correctly removes brazilian phone number mask" <|
            \() ->
                "(47) 91234-1234"
                    |> Mask.remove { mask = "(##) #####-####", replace = '#' }
                    |> Expect.equal "47912341234"
        , test "Correctly masks international phone number" <|
            \() ->
                "5547912341234"
                    |> Mask.string { mask = "+## (##) #####-####", replace = '#' }
                    |> Expect.equal "+55 (47) 91234-1234"
        , test "Correctly removes international phone number mask" <|
            \() ->
                "+55 (47) 91234-1234"
                    |> Mask.remove { mask = "+## (##) #####-####", replace = '#' }
                    |> Expect.equal "5547912341234"
        ]


stringMaskFuzzer : Fuzzer { mask : String, replace : Char }
stringMaskFuzzer =
    Fuzz.custom stringMaskGenerator stringMaskShrinker


stringMaskGenerator : Random.Generator { mask : String, replace : Char }
stringMaskGenerator =
    Random.Char.latin
        |> Random.andThen
            (\randomChar ->
                Random.Extra.choices (Random.constant randomChar) [ Random.Char.latin ]
                    |> Random.Extra.rangeLengthList 5 200
                    |> Random.map
                        (\randomString ->
                            { mask = String.fromList randomString, replace = randomChar }
                        )
            )


stringMaskShrinker : Shrinker { mask : String, replace : Char }
stringMaskShrinker mask_ =
    Shrink.noShrink (\mask replace -> { mask = mask, replace = replace })
        |> Shrink.andMap (Shrink.string mask_.mask)
        |> Shrink.andMap (Shrink.noShrink mask_.replace)
