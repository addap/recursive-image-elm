module Util exposing (..)


range : Int -> List Int
range x =
    if x > 0 then
        range (x - 1) ++ [ x ]

    else
        []


iter : Int -> (a -> a) -> a -> a
iter n f x =
    if n > 0 then
        f (iter (n - 1) f x)

    else
        x


iterCollect : Int -> (a -> a) -> a -> List a
iterCollect n f x =
    if n > 0 then
        let
            rec =
                iterCollect (n - 1) f x
        in
        f x :: List.map f rec

    else
        []


sign : number -> number
sign x =
    if x < 0 then
        -1

    else if x > 0 then
        1

    else
        0
