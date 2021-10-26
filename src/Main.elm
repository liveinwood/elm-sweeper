module Main exposing (..)

import List exposing (concatMap, map)
import Matrix exposing (Matrix, empty, fromLists)
import Maybe exposing (withDefault)



-- MODEL


width : Int
width =
    5


type alias Cell =
    { opened : Bool
    , safe : Bool
    , count : Int
    }


initialModel : Matrix Bool
initialModel =
    withDefault empty (fromLists [ [ True, False, True ], [ False, True, True ], [ False, False, True ] ])


{-| 隣接するセルの座標を返す

    neighbors ( 0, 0 ) =
        [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
    neighbors ( 1, 1 ) =
        [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]

-}
neighbors : ( Int, Int ) -> List ( Int, Int )
neighbors ( r, c ) =
    [ -1, 0, 1 ]
        |> concatMap
            (\x ->
                [ -1, 0, 1 ]
                    |> concatMap
                        (\y ->
                            if x /= 0 || y /= 0 then
                                [ ( x, y ) ]

                            else
                                []
                        )
            )
        |> map (\( x, y ) -> ( r + x, c + y ))
