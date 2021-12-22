module Zipper exposing (Movement(..), Zipper, map, mkZipper, modify, move, toList)

import Html exposing (a)


type alias Zipper a =
    { prev : List a
    , focus : a
    , next : List a
    }


type Movement
    = Prev
    | Next


move : Movement -> Zipper a -> ( Zipper a, Bool )
move m ({ prev, focus, next } as z) =
    case m of
        Prev ->
            case prev of
                [] ->
                    ( z, False )

                p :: prev1 ->
                    ( { prev = prev1, focus = p, next = focus :: next }, True )

        Next ->
            case next of
                [] ->
                    ( z, False )

                n :: next1 ->
                    ( { prev = focus :: prev, focus = n, next = next1 }, True )


mkZipper : a -> List a -> Zipper a
mkZipper x l =
    { prev = [], focus = x, next = l }


toList : Zipper a -> List a
toList { prev, focus, next } =
    List.reverse prev ++ (focus :: next)


map : (a -> b) -> Zipper a -> Zipper b
map f { prev, focus, next } =
    { prev = List.map f prev, focus = f focus, next = List.map f next }


modify : (a -> a) -> Zipper a -> Zipper a
modify f z =
    { z | focus = f z.focus }


getFocus : Zipper a -> a
getFocus { focus } =
    focus
