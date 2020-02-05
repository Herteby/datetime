module DateTime exposing (..)

import Date
import Iso8601
import String.Extra as String
import Time
import Time.Extra


type Yes
    = Yes


type No
    = No


type DateTime units
    = DateTime Time.Posix


type alias Years =
    { year : Yes, month : No, day : No, hour : No, minute : No, second : No, millis : No }


type alias Months =
    { year : Yes, month : Yes, day : No, hour : No, minute : No, second : No, millis : No }


type alias Days =
    { year : Yes, month : Yes, day : Yes, hour : No, minute : No, second : No, millis : No }


type alias Hours =
    { year : Yes, month : Yes, day : Yes, hour : Yes, minute : No, second : No, millis : No }


type alias Minutes =
    { year : Yes, month : Yes, day : Yes, hour : Yes, minute : Yes, second : No, millis : No }


type alias Seconds =
    { year : Yes, month : Yes, day : Yes, hour : Yes, minute : Yes, second : Yes, millis : No }


type alias Millis =
    { year : Yes, month : Yes, day : Yes, hour : Yes, minute : Yes, second : Yes, millis : Yes }


type alias WithYears a =
    { a | year : Yes }


type alias WithMonths a =
    { a | year : Yes, month : Yes }


type alias WithDays a =
    { a | year : Yes, month : Yes, day : Yes }


type alias WithHours a =
    { a | year : Yes, month : Yes, day : Yes, hour : Yes }


type alias WithMinutes a =
    { a | year : Yes, month : Yes, day : Yes, hour : Yes, minute : Yes }


type alias WithSeconds a =
    { a | year : Yes, month : Yes, day : Yes, hour : Yes, minute : Yes, second : Yes }


type alias WithMillis a =
    { a | year : Yes, month : Yes, day : Yes, hour : Yes, minute : Yes, second : Yes, millis : Yes }



-- CREATE


withYear : Int -> DateTime Years
withYear year =
    DateTime <| Time.Extra.add Time.Extra.Year (year - 1970) Time.utc yearZero


withMonth : Time.Month -> DateTime Years -> DateTime Months
withMonth month (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Month (monthToNumber month - 1) Time.utc time


withMonthNumber : Int -> DateTime Years -> DateTime Months
withMonthNumber month (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Month (month - 1) Time.utc time


withDay : Int -> DateTime Months -> DateTime Days
withDay day (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Day (day - 1) Time.utc time


withHour : Int -> DateTime Days -> DateTime Hours
withHour hour (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Hour hour Time.utc time


withMinute : Int -> DateTime Hours -> DateTime Minutes
withMinute minute (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Minute minute Time.utc time


withSecond : Int -> DateTime Minutes -> DateTime Seconds
withSecond second (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Second second Time.utc time


withMillis : Int -> DateTime Seconds -> DateTime Millis
withMillis millis (DateTime time) =
    DateTime <| Time.Extra.add Time.Extra.Millisecond millis Time.utc time


fromMillis : Int -> DateTime Millis
fromMillis millis =
    DateTime <| Time.millisToPosix millis


fromPosix : Time.Posix -> DateTime Millis
fromPosix =
    DateTime



--PARSE


yearsFromString : String -> Maybe (DateTime Years)
yearsFromString string =
    if String.length string == 4 then
        String.toInt string |> Maybe.map withYear

    else
        Nothing


monthsFromIsoString : String -> Maybe (DateTime Months)
monthsFromIsoString string =
    if String.length string == 7 then
        Date.fromIsoString string
            |> Result.toMaybe
            |> Maybe.map
                (\date ->
                    withYear (Date.year date)
                        |> withMonth (Date.month date)
                )

    else
        Nothing


daysFromIsoString : String -> Maybe (DateTime Days)
daysFromIsoString string =
    if String.length string == 10 then
        Date.fromIsoString string
            |> Result.toMaybe
            |> Maybe.map
                (\date ->
                    withYear (Date.year date)
                        |> withMonth (Date.month date)
                        |> withDay (Date.day date)
                )

    else
        Nothing


millisFromIsoString : String -> Maybe (DateTime Millis)
millisFromIsoString =
    Iso8601.toTime >> Result.toMaybe >> Maybe.map fromPosix



-- TO


toYear : DateTime (WithYears a) -> Int
toYear (DateTime time) =
    Time.toYear Time.utc time


toMonth : DateTime (WithMonths a) -> Time.Month
toMonth (DateTime time) =
    Time.toMonth Time.utc time


toMonthNumber : DateTime (WithMonths a) -> Int
toMonthNumber =
    toMonth >> monthToNumber


toDay : DateTime (WithDays a) -> Int
toDay (DateTime time) =
    Time.toDay Time.utc time


toWeekday : DateTime (WithDays a) -> Time.Weekday
toWeekday (DateTime time) =
    Time.toWeekday Time.utc time


toHour : DateTime (WithHours a) -> Int
toHour (DateTime time) =
    Time.toHour Time.utc time


toMinute : DateTime (WithMinutes a) -> Int
toMinute (DateTime time) =
    Time.toMinute Time.utc time


toSecond : DateTime (WithSeconds a) -> Int
toSecond (DateTime time) =
    Time.toSecond Time.utc time


toMillis : DateTime (WithMillis a) -> Int
toMillis (DateTime time) =
    Time.toMillis Time.utc time



-- MANIPULATE


addYears : Int -> DateTime (WithYears a) -> DateTime (WithYears a)
addYears =
    add Time.Extra.Year


addMonths : Int -> DateTime (WithMonths a) -> DateTime (WithMonths a)
addMonths =
    add Time.Extra.Month


addDays : Int -> DateTime (WithDays a) -> DateTime (WithDays a)
addDays =
    add Time.Extra.Day


addHours : Int -> DateTime (WithHours a) -> DateTime (WithHours a)
addHours =
    add Time.Extra.Hour


addMinutes : Int -> DateTime (WithMinutes a) -> DateTime (WithMinutes a)
addMinutes =
    add Time.Extra.Minute


addSeconds : Int -> DateTime (WithSeconds a) -> DateTime (WithSeconds a)
addSeconds =
    add Time.Extra.Second


addMilliseconds : Int -> DateTime (WithMillis a) -> DateTime (WithMillis a)
addMilliseconds =
    add Time.Extra.Millisecond


{-| Internal
-}
add : Time.Extra.Interval -> Int -> DateTime a -> DateTime a
add interval int (DateTime time) =
    DateTime <| Time.Extra.add interval int Time.utc time



-- DIFF


diffYears : DateTime (WithYears a) -> DateTime (WithYears a) -> Int
diffYears =
    diff Time.Extra.Year


diffMonths : DateTime (WithMonths a) -> DateTime (WithMonths a) -> Int
diffMonths =
    diff Time.Extra.Month


diffDays : DateTime (WithDays a) -> DateTime (WithDays a) -> Int
diffDays =
    diff Time.Extra.Day


diffHours : DateTime (WithHours a) -> DateTime (WithHours a) -> Int
diffHours =
    diff Time.Extra.Hour


diffMinutes : DateTime (WithMinutes a) -> DateTime (WithMinutes a) -> Int
diffMinutes =
    diff Time.Extra.Minute


diffSeconds : DateTime (WithSeconds a) -> DateTime (WithSeconds a) -> Int
diffSeconds =
    diff Time.Extra.Second


diffMilliseconds : DateTime (WithMillis a) -> DateTime (WithMillis a) -> Int
diffMilliseconds =
    diff Time.Extra.Millisecond


{-| Internal
-}
diff : Time.Extra.Interval -> DateTime a -> DateTime a -> Int
diff interval (DateTime time1) (DateTime time2) =
    Time.Extra.diff interval Time.utc time1 time2



-- MISC


withZone : Time.Zone -> DateTime a -> DateTime a
withZone zone (DateTime time) =
    DateTime <| Time.millisToPosix <| Time.toMillis zone time


yearZero : Time.Posix
yearZero =
    Time.millisToPosix 0


monthToNumber : Time.Month -> number
monthToNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
