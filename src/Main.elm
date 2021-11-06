module Main exposing (..)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as H exposing (width)
import Html.Events.Extra.Mouse as MS
import List.Extra as LE
import Matrix as M exposing (Matrix)
import Random as R exposing (Generator)
import Random.Array as RA
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


type CellStatus
    = OPENED
    | FLAGED
    | CLOSED


type GameStatus
    = PLAYING
    | WIN
    | LOOSE


type alias Model =
    { status : GameStatus
    , board : Matrix Cell
    , safeCellCount : Int
    , openedCellCount : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = PLAYING
      , board = M.empty
      , safeCellCount = 0
      , openedCellCount = 0
      }
    , R.generate InitMatrix random2DBoolGenerator
    )



-- UPDATE


type Msg
    = InitMatrix (Array (Array Bool))
    | Open ( Int, Int )



-- | Open ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitMatrix array ->
            ( { status = PLAYING
              , board = initialize array
              , safeCellCount = totalSafeCellCount array
              , openedCellCount = 0
              }
            , Cmd.none
            )

        Open ( x, y ) ->
            ( open ( x, y ) model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    H.div []
        [ table (toList model.board)
        , H.p []
            [ if model.status == PLAYING then
                -- H.text ("safe cell count = " ++ String.fromInt model.safeCellCount ++ " opened cell count = " ++ String.fromInt model.openedCellCount)
                H.text ""

              else if model.status == WIN then
                H.text "GAME CLEAR"

              else
                H.text "GAME OVER"
            ]
        ]


table : List (List Cell) -> Html Msg
table lists =
    H.table
        []
        (List.map tr lists)


tr : List Cell -> Html Msg
tr cells =
    H.tr [] (List.map td cells)


td : Cell -> Html Msg
td cell =
    let
        color =
            if cell.status == CLOSED then
                "#808080"

            else
                "C0C0C0"
    in
    H.td
        [ H.style "text-align" "center"
        , H.style "vertical-align" "middle"
        , H.style "border" "1px #2b2b2b solid"
        , H.style "width" "28px"
        , H.style "height" "28px"
        , H.style "background-color" color
        ]
        [ if cell.status == OPENED then
            H.text
                (if cell.safe then
                    if cell.count > 0 then
                        String.fromInt cell.count

                    else
                        ""

                 else
                    "●"
                )

          else if cell.status == CLOSED then
            H.button
                [ H.style "width" "100%"
                , H.style "height" "100%"
                , MS.onClick (\_ -> Open cell.pos)
                ]
                []

          else
            H.text ""
        ]


width : Int
width =
    10


randomBoolGenerator : Generator Bool
randomBoolGenerator =
    R.map (\i -> i >= 1) (R.int 0 5)


random2DBoolGenerator : Generator (Array (Array Bool))
random2DBoolGenerator =
    RA.array width (RA.array width randomBoolGenerator)


getWithDefault : a -> Array (Array a) -> ( Int, Int ) -> a
getWithDefault d array2d ( x, y ) =
    get array2d ( x, y ) |> Maybe.withDefault d


totalSafeCellCount : Array (Array Bool) -> Int
totalSafeCellCount =
    totalCount (\a -> a == True)


totalOpenedCount : Array (Array Cell) -> Int
totalOpenedCount =
    totalCount (\c -> c.status == OPENED)


totalCount : (a -> Bool) -> Array (Array a) -> Int
totalCount pred m =
    m
        |> Array.map
            (Array.foldl
                (\a ->
                    \n ->
                        n
                            + (if pred a then
                                1

                               else
                                0
                              )
                )
                0
            )
        |> Array.foldl (+) 0


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
    M.initialize h w (\x y -> Cell ( x, y ) CLOSED (getWithDefault False array2d ( x, y )) (aroundFalseCount ( x, y ) array2d))


open : ( Int, Int ) -> Model -> Model
open ( x, y ) model =
    case M.get model.board x y of
        Just c ->
            if c.safe then
                openAt ( x, y ) model

            else
                { model | board = openAllCell model.board, status = LOOSE }

        Nothing ->
            model


openAllCell : Matrix Cell -> Matrix Cell
openAllCell m =
    M.map (\c -> { c | status = OPENED }) m


openAt : ( Int, Int ) -> Model -> Model
openAt ( x, y ) model =
    let
        opened =
            toBeOpened ( x, y ) S.empty model.board

        newBoard =
            M.indexedMap (openCell opened) model.board

        newOpenedCellCount =
            totalOpenedCount newBoard

        newGameStatus =
            if model.safeCellCount == newOpenedCellCount then
                WIN

            else
                PLAYING
    in
    { model | board = newBoard, openedCellCount = newOpenedCellCount, status = newGameStatus }


openCell : Set ( Int, Int ) -> Int -> Int -> Cell -> Cell
openCell opens x y c =
    if S.member ( x, y ) opens then
        { c | status = OPENED }

    else
        c


toBeOpened : ( Int, Int ) -> Set ( Int, Int ) -> Matrix Cell -> Set ( Int, Int )
toBeOpened ( x, y ) acc m =
    let
        cell =
            M.get m x y
    in
    case cell of
        Just c ->
            if c.count > 0 then
                S.singleton ( x, y )
                -- S.empty

            else
                let
                    nei =
                        S.fromList (neighboursXY ( x, y ) m)

                    opened =
                        S.union (S.singleton ( x, y )) nei

                    next =
                        S.diff nei acc

                    acc2 =
                        S.union acc opened
                in
                S.union opened (SE.concatMap (\( a, b ) -> toBeOpened ( a, b ) acc2 m) next)

        Nothing ->
            S.empty


toList : Matrix Cell -> List (List Cell)
toList m =
    Array.map Array.toList m |> Array.toList


type alias Cell =
    { pos : ( Int, Int )
    , status : CellStatus
    , safe : Bool
    , count : Int
    }
