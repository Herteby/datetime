module Examples exposing (main)

import DateTime exposing (..)
import DateTime.Format exposing (..)
import Html exposing (Html)
import Time


{-| 1989-08-30
-}
toIsoString : DateTime (WithDays a) -> String
toIsoString =
    format [ year, text "-", monthNumber, text "-", day ]


{-| 1989-08-30 13:37
-}
toDateTimeString : DateTime (WithMinutes a) -> String
toDateTimeString =
    format [ year, text "-", monthNumber, text "-", day, text " ", hour, text ":", minute ]


main : Html msg
main =
    showStrings
        [ withYear 1998
            |> withMonthNumber 5
            |> withDay 15
            |> format
                [ year
                , text "-"
                , monthNumber
                , text "-"
                , day
                ]
        , withYear 1998
            |> addYears 5
            |> format [ year ]
        , withYear 1989
            |> withMonth Time.Aug
            |> withDay 30
            |> DateTime.addYears 30
            |> toIsoString
        ]


showStrings : List String -> Html msg
showStrings =
    List.map (\string -> Html.li [] [ Html.text string ]) >> Html.ul []
