module MainTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Matrix as M exposing (Matrix)
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
                        Array.fromList [ Cell False True 2, Cell False False 2, Cell False True 2 ]
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
                        Array.fromList [ Cell False False 2, Cell False True 4, Cell False False 2 ]
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
                        Array.fromList [ Cell False True 2, Cell False False 2, Cell False True 2 ]
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
                            [ Array.fromList [ Cell False True 2, Cell False False 2, Cell False True 3, Cell False False 1 ]
                            , Array.fromList [ Cell False False 2, Cell False True 4, Cell False False 4, Cell False True 3 ]
                            , Array.fromList [ Cell False True 3, Cell False False 4, Cell False True 4, Cell False False 2 ]
                            , Array.fromList [ Cell False False 1, Cell False True 3, Cell False False 2, Cell False True 2 ]
                            ]
                in
                Expect.equal matrix expect
        ]
