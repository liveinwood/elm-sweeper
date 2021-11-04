module Main exposing (..)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import List.Extra as LE
import Matrix as M exposing (Matrix)
import Random as R exposing (Generator)
import Random.Array as RA
import Random.Char exposing (yijingHexagramSymbol)
import Set as S exposing (Set)
import Set.Extra as SE



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    Matrix Cell


init : () -> ( Model, Cmd Msg )
init _ =
    ( M.empty, R.generate InitMatrix random2DBoolGenerator )



-- UPDATE


type Msg
    = InitMatrix (Array (Array Bool))



-- | Open ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitMatrix array ->
            ( initialize array, Cmd.none )



-- Open ( x, y ) ->
--     ( open ( x, y ) model, Cmd.none )
-- VIEW


view : Model -> Html Msg
view model =
    H.div [] [ table (toList model) ]


table : List (List Cell) -> Html Msg
table lists =
    H.table [] (List.map tr lists)


tr : List Cell -> Html Msg
tr cells =
    H.tr [] (List.map td cells)


td : Cell -> Html Msg
td cell =
    H.td []
        [ if cell.safe then
            H.text (String.fromInt cell.count)

          else
            H.text "●"
        ]


width : Int
width =
    5


randomBoolGenerator : Generator Bool
randomBoolGenerator =
    R.map (\i -> i == 0) (R.int 0 1)


random2DBoolGenerator : Generator (Array (Array Bool))
random2DBoolGenerator =
    RA.array width (RA.array width randomBoolGenerator)


getWithDefault : a -> Array (Array a) -> ( Int, Int ) -> a
getWithDefault d array2d ( x, y ) =
    get array2d ( x, y ) |> Maybe.withDefault d


get : Array (Array a) -> ( Int, Int ) -> Maybe a
get array2d ( x, y ) =
    Array.get x array2d |> Maybe.andThen (Array.get y)


neighboursXY : ( Int, Int ) -> Array (Array a) -> List ( Int, Int )
neighboursXY ( x, y ) array2d =
    [ -1, 0, 1 ]
        |> LE.andThen
            (\a ->
                [ -1, 0, 1 ]
                    |> LE.andThen
                        (\b ->
                            if a /= 0 || b /= 0 then
                                case get array2d ( x + a, y + b ) of
                                    Just _ ->
                                        [ ( x + a, y + b ) ]

                                    Nothing ->
                                        []

                            else
                                []
                        )
            )


neighbours : ( Int, Int ) -> Array (Array a) -> List a
neighbours ( x, y ) array2d =
    [ -1, 0, 1 ]
        |> LE.andThen
            (\a ->
                [ -1, 0, 1 ]
                    |> LE.andThen
                        (\b ->
                            if a /= 0 || b /= 0 then
                                case get array2d ( x + a, y + b ) of
                                    Just z ->
                                        [ z ]

                                    Nothing ->
                                        []

                            else
                                []
                        )
            )


aroundFalseCount : ( Int, Int ) -> Array (Array Bool) -> Int
aroundFalseCount ( x, y ) array2d =
    neighbours ( x, y ) array2d |> List.filter (\b -> not b) |> List.length


initialize : Array (Array Bool) -> Matrix Cell
initialize array2d =
    let
        h =
            Array.length array2d

        w =
            Array.length (Maybe.withDefault Array.empty (Array.get 0 array2d))
    in
    M.initialize h w (\x y -> Cell False (getWithDefault False array2d ( x, y )) (aroundFalseCount ( x, y ) array2d))



-- open : (Int, Int) -> Matrix Cell -> Matrix Cell
-- open (x, y) m =


toBeOpened : ( Int, Int ) -> Set ( Int, Int ) -> Matrix Cell -> Set ( Int, Int )
toBeOpened ( x, y ) acc m =
    let
        cell =
            M.get m x y
    in
    case cell of
        Just c ->
            if c.count > 0 then
                -- S.singleton ( x, y )
                S.empty

            else
                let
                    opened =
                        S.diff (S.union (S.singleton ( x, y )) (S.fromList (neighboursXY ( x, y ) m))) acc
                in
                S.union opened (SE.concatMap (\( a, b ) -> toBeOpened ( a, b ) opened m) opened)

        Nothing ->
            S.empty


toList : Matrix Cell -> List (List Cell)
toList m =
    Array.map Array.toList m |> Array.toList


type alias Cell =
    { opened : Bool
    , safe : Bool
    , count : Int
    }
