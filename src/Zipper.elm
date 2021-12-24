module Zipper exposing (Mode(..), Movement(..), Zipper, getFirst, getFocus, length, map, mapi, mkZipper, modify, move, toList, toListMode)

import Browser.Dom exposing (focus)
import Util exposing (list_mapi)


{-| Zipper datastructure to do efficient forward and backwards movement.
-}
type alias Zipper a =
    { prev : List a
    , focus : a
    , next : List a
    }


type Mode
    = Other
    | Focus


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


toListMode : Zipper a -> List ( Mode, a )
toListMode z =
    toList (mmap Tuple.pair z)


map : (a -> b) -> Zipper a -> Zipper b
map f { prev, focus, next } =
    { prev = List.map f prev, focus = f focus, next = List.map f next }


mmap : (Mode -> a -> b) -> Zipper a -> Zipper b
mmap f { prev, focus, next } =
    { prev = List.map (f Other) prev, focus = f Focus focus, next = List.map (f Other) next }


mapi : (Mode -> Int -> a -> b) -> Zipper a -> Zipper b
mapi f { prev, focus, next } =
    let
        nprev =
            List.length prev

        f2 =
            \m n a -> f m (n + nprev + 1) a
    in
    { prev = List.reverse (list_mapi (f Other) (List.reverse prev)), focus = f Focus nprev focus, next = list_mapi (f2 Other) next }


modify : (a -> a) -> Zipper a -> Zipper a
modify f z =
    { z | focus = f z.focus }


getFocus : Zipper a -> a
getFocus { focus } =
    focus


getFirst : Zipper a -> a
getFirst { prev, focus } =
    case List.reverse prev of
        [] ->
            focus

        fst :: _ ->
            fst


length : Zipper a -> Int
length { prev, next } =
    List.length prev + List.length next + 1
