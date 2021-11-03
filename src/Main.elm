module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (update)
import Html as H exposing (Html)
import List.Extra as LE
import Matrix as M exposing (Matrix)
import Random as R exposing (Generator)
import Random.Array as RA



-- MAIN


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitMatrix array ->
            ( initialize array, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    H.div [] []


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


neighbours : ( Int, Int ) -> Array (Array Bool) -> List Bool
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


toArray : Matrix Cell -> List (Array Cell)
toArray m =
    let
        ( rows, _ ) =
            M.size m
    in
    List.map (M.getXs m) (List.range 0 (rows - 1))


type alias Cell =
    { opened : Bool
    , safe : Bool
    , count : Int
    }
