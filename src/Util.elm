module Util exposing (..)

{-| Utility functions.
-}


{-| Compute the range [0, ..., n]
-}
range : Int -> List Int
range n =
    if n > 0 then
        range (n - 1) ++ [ n ]

    else
        []


{-| Compute the iteration f^n x
-}
iter : Int -> (a -> a) -> a -> a
iter n f x =
    if n > 0 then
        f (iter (n - 1) f x)

    else
        x


{-| Compute the iteration with saved intermediates [x, f x, ..., f^n x]
-}
iterCollect : Int -> (a -> a) -> a -> List a
iterCollect n f x =
    if n > 0 then
        let
            rec =
                iterCollect (n - 1) f x
        in
        x :: List.map f rec

    else
        [ x ]


trunc_tail : List a -> List a
trunc_tail l =
    case l of
        [] ->
            []

        _ :: t ->
            t


{-| Return -1 if number is negative, 1 if number is positive and 0 otherwise.
-}
sign : number -> number
sign x =
    if x < 0 then
        -1

    else if x > 0 then
        1

    else
        0
