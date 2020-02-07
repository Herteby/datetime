module DateTime.Format exposing (..)

import DateTime exposing (..)


type Token a
    = Token (DateTime a -> String)


format : List (Token a) -> DateTime a -> String
format tokens dateTime =
    List.foldl (\(Token fn) acc -> acc ++ fn dateTime) "" tokens



-- Tokens


year : Token (WithYears a)
year =
    Token <| toYear >> String.fromInt


yearLastTwo : Token (WithYears a)
yearLastTwo =
    Token <| toYear >> String.fromInt >> String.right 2


monthNumber : Token (WithMonths a)
monthNumber =
    Token <| toMonth >> monthToNumber >> String.fromInt >> String.padLeft 2 '0'


day : Token (WithDays a)
day =
    Token <| toDay >> String.fromInt >> String.padLeft 2 '0'


hour : Token (WithHours { a | zone : Yes })
hour =
    Token <| toHour >> String.fromInt >> String.padLeft 2 '0'


minute : Token (WithMinutes { a | zone : Yes })
minute =
    Token <| toMinute >> String.fromInt >> String.padLeft 2 '0'


second : Token (WithSeconds { a | zone : Yes })
second =
    Token <| toSecond >> String.fromInt >> String.padLeft 2 '0'


text : String -> Token a
text string =
    Token <| always string
