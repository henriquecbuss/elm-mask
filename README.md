# NeoVier/elm-mask

[![Build Status](https://github.com/NeoVier/elm-mask/workflows/CI/badge.svg)](https://github.com/NeoVier/elm-mask/actions?query=branch%3Amain) [![Elm package](https://img.shields.io/elm-package/v/NeoVier/elm-mask.svg)](https://package.elm-lang.org/packages/NeoVier/elm-mask/latest/)

Mask `String`s to be used in input fields. Apply and remove masks so it's easy
to use in your update and view functions!

See an [example in action on Ellie](https://ellie-app.com/f9Zdmvnfwxfa1).

See more end-to-end example code in the `examples/` folder.

## Overview

```elm
Html.form []
    [ Html.input
        [ model.phoneNumber
            -- You can also apply the mask on your `update` function
            |> Mask.string { mask = "(##) #####-####", replace = '#' }
            |> Html.Attributes.value
        , Html.Events.onInput EnteredPhoneNumber
        ]
        []
    , Html.input
        [ model.price
            |> Mask.float (Mask.Precisely 2)
            |> Html.Attributes.value
        , Html.Events.onInput EnteredExampleFloatInput
        ]
        []
    ]
```

## Usage

Just apply a mask on your input's `value`, and remove the mask before storing
the value on your backend!

## Learning Resources

Feel free to ask for help on the [Elm Slack](https://elmlang.herokuapp.com/),
in a channel or DMing me @Henrique Buss, any feedback is also welcome! And if
you find any bugs, feel free to submit an issue!
