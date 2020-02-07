module Examples exposing (main)

import DateTime exposing (DateTime, No, WithDays, WithMinutes, Yes)
import DateTime.Format as Format
import Html exposing (Html)
import Time
import TimeZone


{-| 1989-08-30
-}
toIsoString : DateTime (WithDays a) -> String
toIsoString =
    Format.format [ Format.year, Format.text "-", Format.monthNumber, Format.text "-", Format.day ]


{-| 1989-08-30 13:37
-}
toDateTimeString : DateTime (WithMinutes { a | zone : Yes }) -> String
toDateTimeString =
    Format.format [ Format.year, Format.text "-", Format.monthNumber, Format.text "-", Format.day, Format.text " ", Format.hour, Format.text ":", Format.minute ]


main : Html msg
main =
    let
        a =
            DateTime.withYear 1989
                |> DateTime.withMonthNumber 8
                |> DateTime.withDay 30
                |> DateTime.withHour 13
                |> DateTime.withMinute 37
    in
    showStrings
        [ DateTime.diffYears
            (DateTime.withYear 1989 |> DateTime.withMonth Time.Aug)
            (DateTime.withYear 2020)
            |> String.fromInt
        , toIsoString a
        , a |> DateTime.withZone Time.utc |> toDateTimeString
        , a |> DateTime.withZone (TimeZone.europe__stockholm ()) |> toDateTimeString
        , a
            |> DateTime.addYears 30
            |> DateTime.withZone Time.utc
            |> toIsoString
        , DateTime.diffMonths (DateTime.withZone Time.utc a) (DateTime.addYears 1 a |> DateTime.withZone Time.utc) |> String.fromInt
        ]


showStrings : List String -> Html msg
showStrings =
    List.map (\string -> Html.li [] [ Html.text string ]) >> Html.ul []
