module Main exposing (..)

import Array exposing (Array)
import List.Extra as LE
import Matrix as M exposing (Matrix)
import Random as R exposing (Generator)
import Random.Array as RA



-- MODEL


width : Int
width =
    5


randomBoolGenerator : Generator Bool
randomBoolGenerator =
    R.map (\i -> i == 0) (R.int 0 1)


random2DBoolGenerator : Generator (Array (Array Bool))
random2DBoolGenerator =
    RA.array width (RA.array width randomBoolGenerator)


get : Array (Array Bool) -> ( Int, Int ) -> Bool
get array2d ( x, y ) =
    Array.get x array2d |> Maybe.andThen (Array.get y) |> Maybe.withDefault False


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours ( x, y ) =
    [ -1, 0, 1 ]
        |> LE.andThen
            (\a ->
                [ -1, 0, 1 ]
                    |> LE.andThen
                        (\b ->
                            if a == 0 && b == 0 then
                                []

                            else
                                [ ( x + a, y + b ) ]
                        )
            )


aroundFalseCount : ( Int, Int ) -> Array (Array Bool) -> Int
aroundFalseCount ( x, y ) array2d =
    neighbours ( x, y ) |> List.map (get array2d) |> List.filter (\b -> b == False) |> List.length


initialize : Array (Array Bool) -> Matrix Cell
initialize array2d =
    M.initialize width width (\x y -> Cell False (get array2d ( x, y )) (aroundFalseCount ( x, y ) array2d))


type alias Cell =
    { opened : Bool
    , safe : Bool
    , count : Int
    }
