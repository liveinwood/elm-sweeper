module MainTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Matrix as M exposing (Matrix)
import Set
import Test exposing (..)


make2dArrayFrom2dList : List (List a) -> Array (Array a)
make2dArrayFrom2dList list =
    list |> List.map Array.fromList |> Array.fromList


array1 : Array (Array Bool)
array1 =
    make2dArrayFrom2dList
        [ [ True, True, True ]
        , [ True, True, True ]
        , [ True, True, True ]
        ]


test1 : Test
test1 =
    describe "aroundFalseCount Test1"
        [ test "test1" <|
            \_ -> Expect.equal (aroundFalseCount ( 0, 0 ) array1) 0
        , test "test2" <|
            \_ -> Expect.equal (aroundFalseCount ( 0, 2 ) array1) 0
        , test "test3" <|
            \_ -> Expect.equal (aroundFalseCount ( 2, 0 ) array1) 0
        , test "test4" <|
            \_ -> Expect.equal (aroundFalseCount ( 2, 2 ) array1) 0
        , test "test5" <|
            \_ -> Expect.equal (aroundFalseCount ( 1, 1 ) array1) 0
        ]


array2 : Array (Array Bool)
array2 =
    make2dArrayFrom2dList
        [ [ True, False, True ]
        , [ False, False, False ]
        , [ True, False, True ]
        ]


test2 : Test
test2 =
    describe "arouncdFalseCount test2"
        [ test "test1" <|
            \_ -> Expect.equal (aroundFalseCount ( 0, 0 ) array2) 3
        , test "test2" <|
            \_ -> Expect.equal (aroundFalseCount ( 0, 2 ) array2) 3
        , test "test3" <|
            \_ -> Expect.equal (aroundFalseCount ( 2, 0 ) array2) 3
        , test "test4" <|
            \_ -> Expect.equal (aroundFalseCount ( 2, 2 ) array2) 3
        , test "test5" <|
            \_ -> Expect.equal (aroundFalseCount ( 1, 1 ) array2) 4
        , test "test6" <|
            \_ -> Expect.equal (aroundFalseCount ( 2, 1 ) array2) 3
        , test "test7" <|
            \_ -> Expect.equal (aroundFalseCount ( 1, 2 ) array2) 3
        , test "test8" <|
            \_ -> Expect.equal (aroundFalseCount ( 1, 0 ) array2) 3
        , test "test9" <|
            \_ -> Expect.equal (aroundFalseCount ( 2, 0 ) array2) 3
        ]


array3 : Array (Array Bool)
array3 =
    make2dArrayFrom2dList
        [ [ True, False, True ]
        , [ False, True, False ]
        , [ True, False, True ]
        ]


test3 : Test
test3 =
    describe "initialize test1"
        [ test "test1" <|
            \_ ->
                let
                    matrix =
                        initialize array3
                in
                Expect.equal (M.size matrix) ( 3, 3 )
        , test "test2" <|
            \_ ->
                let
                    row0 =
                        M.getXs (initialize array3) 0

                    expect =
                        Array.fromList [ Cell ( 0, 0 ) CLOSED True 2, Cell ( 0, 1 ) CLOSED False 2, Cell ( 0, 2 ) CLOSED True 2 ]
                in
                Expect.equal row0 expect
        , test "test3" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, False, True ]
                            , [ False, True, False ]
                            , [ True, False, True ]
                            ]

                    row1 =
                        M.getXs (initialize array) 1

                    expect =
                        Array.fromList [ Cell ( 1, 0 ) CLOSED False 2, Cell ( 1, 1 ) CLOSED True 4, Cell ( 1, 2 ) CLOSED False 2 ]
                in
                Expect.equal row1 expect
        , test "test4" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, False, True ]
                            , [ False, True, False ]
                            , [ True, False, True ]
                            ]

                    row2 =
                        M.getXs (initialize array) 2

                    expect =
                        Array.fromList [ Cell ( 2, 0 ) CLOSED True 2, Cell ( 2, 1 ) CLOSED False 2, Cell ( 2, 2 ) CLOSED True 2 ]
                in
                Expect.equal row2 expect
        , test "test5" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, False, True, False ]
                            , [ False, True, False, True ]
                            , [ True, False, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    expect =
                        Array.fromList
                            [ Array.fromList [ Cell ( 0, 0 ) CLOSED True 2, Cell ( 0, 1 ) CLOSED False 2, Cell ( 0, 2 ) CLOSED True 3, Cell ( 0, 3 ) CLOSED False 1 ]
                            , Array.fromList [ Cell ( 1, 0 ) CLOSED False 2, Cell ( 1, 1 ) CLOSED True 4, Cell ( 1, 2 ) CLOSED False 4, Cell ( 1, 3 ) CLOSED True 3 ]
                            , Array.fromList [ Cell ( 2, 0 ) CLOSED True 3, Cell ( 2, 1 ) CLOSED False 4, Cell ( 2, 2 ) CLOSED True 4, Cell ( 2, 3 ) CLOSED False 2 ]
                            , Array.fromList [ Cell ( 3, 0 ) CLOSED False 1, Cell ( 3, 1 ) CLOSED True 3, Cell ( 3, 2 ) CLOSED False 2, Cell ( 3, 3 ) CLOSED True 2 ]
                            ]
                in
                Expect.equal matrix expect
        ]


test4 : Test
test4 =
    describe
        "toBeOpened test1"
        [ test "test1" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, False, True, False ]
                            , [ False, True, False, True ]
                            , [ True, False, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 0 ) Set.empty matrix

                    expect =
                        Set.singleton ( 0, 0 )
                in
                Expect.equalSets expect output
        , test "test2" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, False, True, False ]
                            , [ False, True, False, True ]
                            , [ True, False, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 2 ) Set.empty matrix

                    expect =
                        Set.singleton ( 0, 2 )
                in
                Expect.equalSets expect output
        , test "test3" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, False, True, False ]
                            , [ False, True, False, True ]
                            , [ True, False, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 2, 2 ) Set.empty matrix

                    expect =
                        Set.singleton ( 2, 2 )
                in
                Expect.equalSets expect output
        , test "test4" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, False ]
                            , [ True, True, False, True ]
                            , [ True, False, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 0 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]
                in
                Expect.equalSets expect output
        , test "test5" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, False ]
                            , [ True, True, True, True ]
                            , [ True, True, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 0 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                in
                Expect.equalSets expect output
        , test "test6" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, False ]
                            , [ True, True, True, True ]
                            , [ True, True, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 1 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                in
                Expect.equalSets expect output
        , test "test7" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, False ]
                            , [ True, True, True, True ]
                            , [ True, True, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 1, 0 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                in
                Expect.equalSets expect output
        , test "test8" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, False ]
                            , [ True, True, True, True ]
                            , [ True, True, True, False ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 1, 1 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
                in
                Expect.equalSets expect output
        , test "test9" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, True, True ]
                            , [ True, True, True, True ]
                            , [ False, True, False, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 0 ) Set.empty matrix

                    expect =
                        Set.fromList
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 1, 3 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            , ( 2, 3 )
                            ]
                in
                Expect.equalSets expect output
        , test "test10" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, True, True ]
                            , [ True, True, True, True ]
                            , [ True, True, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 0, 0 ) Set.empty matrix

                    expect =
                        Set.fromList
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 1, 3 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            , ( 2, 3 )
                            , ( 3, 0 )
                            , ( 3, 1 )
                            , ( 3, 2 )
                            , ( 3, 3 )
                            ]
                in
                Expect.equalSets expect output
        , test "test11" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, True, True ]
                            , [ True, True, True, True ]
                            , [ True, True, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 3, 3 ) Set.empty matrix

                    expect =
                        Set.fromList
                            [ ( 0, 0 )
                            , ( 0, 1 )
                            , ( 0, 2 )
                            , ( 0, 3 )
                            , ( 1, 0 )
                            , ( 1, 1 )
                            , ( 1, 2 )
                            , ( 1, 3 )
                            , ( 2, 0 )
                            , ( 2, 1 )
                            , ( 2, 2 )
                            , ( 2, 3 )
                            , ( 3, 0 )
                            , ( 3, 1 )
                            , ( 3, 2 )
                            , ( 3, 3 )
                            ]
                in
                Expect.equalSets expect output
        , test "test12" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, True, False ]
                            , [ True, True, True, True ]
                            , [ True, True, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 2, 2 ) Set.empty matrix

                    expect =
                        Set.singleton ( 2, 2 )
                in
                Expect.equalSets expect output
        , test "test13" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, False, False ]
                            , [ True, False, True, True ]
                            , [ True, False, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 3, 3 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 2, 2 ), ( 2, 3 ), ( 3, 2 ), ( 3, 3 ) ]
                in
                Expect.equalSets expect output
        , test "test14" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, False, False ]
                            , [ False, True, True, True ]
                            , [ False, True, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 3, 3 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 2, 1 ), ( 2, 2 ), ( 2, 3 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ) ]
                in
                Expect.equalSets expect output
        , test "test15" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, False, False ]
                            , [ False, True, True, True ]
                            , [ False, True, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 2, 1 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 2, 1 ) ]
                in
                Expect.equalSets expect output
        , test "test16" <|
            \_ ->
                let
                    array =
                        make2dArrayFrom2dList
                            [ [ True, True, True, True ]
                            , [ True, True, False, False ]
                            , [ False, True, True, True ]
                            , [ False, True, True, True ]
                            ]

                    matrix =
                        initialize array

                    output =
                        toBeOpened ( 3, 2 ) Set.empty matrix

                    expect =
                        Set.fromList [ ( 2, 1 ), ( 2, 2 ), ( 2, 3 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ) ]
                in
                Expect.equalSets expect output
        ]
