module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Dict
import Histogrammar
import Histogrammar.Utils as Utils
import Json.Decode as JD


expectDecodedEquals : JD.Decoder a -> String -> a -> Expect.Expectation
expectDecodedEquals decoder json expectedData =
    JD.decodeString decoder json
        |> Utils.unifyResult Expect.fail (Expect.equal expectedData)


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Decode Histogrammar JSON"
            [ test "Counted" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Count\",\"data\": 123.0}"

                        expectedHistogram =
                            Histogrammar.Counted { entries = 123.0 }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "Summed" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Sum\",\"data\":{\"entries\":123.0,\"sum\":3.14,\"name\":\"myfunc\"}}"

                        expectedHistogram =
                            Histogrammar.Summed { name = Just "myfunc", entries = 123.0, sum = 3.14 }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "Averaged" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Average\",\"data\":{\"entries\":123,\"mean\":3.14,\"name\":\"myfunc\"}}"

                        expectedHistogram =
                            Histogrammar.Averaged { name = Just "myfunc", entries = 123.0, mean = 3.14 }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "Deviated" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Deviate\",\"data\":{\"entries\":123,\"mean\":3.14,\"variance\":0.1,\"name\":\"myfunc\"}}"

                        expectedHistogram =
                            Histogrammar.Deviated
                                { name = Just "myfunc"
                                , entries = 123.0
                                , mean = 3.14
                                , variance = 0.1
                                }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "Minimized" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Minimize\",\"data\":{\"entries\":123,\"min\":3.14,\"name\":\"myfunc\"}}"

                        expectedHistogram =
                            Histogrammar.Minimized
                                { name = Just "myfunc"
                                , entries = 123.0
                                , min = 3.14
                                }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "Maximized" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Maximize\",\"data\":{\"entries\":123,\"max\":3.14,\"name\":\"myfunc\"}}"

                        expectedHistogram =
                            Histogrammar.Maximized
                                { name = Just "myfunc"
                                , entries = 123.0
                                , max = 3.14
                                }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , describe "Bagged"
                [ test "BagOfFloatToFloat" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"Bag\",\"data\":{\"entries\":123,\"values\":[{\"w\":23,\"v\":-999},{\"w\":20,\"v\":-4},{\"w\":20,\"v\":-2},{\"w\":30,\"v\":0},{\"w\":30,\"v\":2}],\"range\":\"N\"}}"

                            expectedHistogram =
                                Histogrammar.Bagged
                                    { name = Nothing
                                    , entries = 123.0
                                    , values =
                                        [ ( -999.0, 23.0 )
                                        , ( -4.0, 20.0 )
                                        , ( -2.0, 20.0 )
                                        , ( 0.0, 30.0 )
                                        , ( 2.0, 30.0 )
                                        ]
                                            |> Dict.fromList
                                            |> Histogrammar.BagOfFloatToFloat
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "BagOfFloatToFloatVector" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"Bag\",\"data\":{\"entries\":123,\"values\":[{\"w\":23,\"v\":[1,2,3]},{\"w\":20,\"v\":[3.14,3.14,3.14]},{\"w\":20,\"v\":[99,50,1]},{\"w\":30,\"v\":[7,2.2,9.8]},{\"w\":30,\"v\":[33.3,66.6,99.9]}],\"range\":\"N3\"}}"

                            expectedHistogram =
                                Histogrammar.Bagged
                                    { name = Nothing
                                    , entries = 123.0
                                    , values =
                                        [ ( [ 1.0, 2.0, 3.0 ], 23.0 )
                                        , ( [ 3.14, 3.14, 3.14 ], 20.0 )
                                        , ( [ 99.0, 50.0, 1.0 ], 20.0 )
                                        , ( [ 7.0, 2.2, 9.8 ], 30.0 )
                                        , ( [ 33.3, 66.6, 99.9 ], 30.0 )
                                        ]
                                            |> Dict.fromList
                                            |> Histogrammar.BagOfFloatToFloatVector 3
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "BagOfFloatToString" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"Bag\",\"data\":{\"entries\":123,\"values\":[{\"w\":23,\"v\":\"five\"},{\"w\":20,\"v\":\"four\"},{\"w\":20,\"v\":\"one\"},{\"w\":30,\"v\":\"three\"},{\"w\":30,\"v\":\"two\"}],\"range\":\"S\"}}"

                            expectedHistogram =
                                Histogrammar.Bagged
                                    { name = Nothing
                                    , entries = 123.0
                                    , values =
                                        [ ( "five", 23.0 )
                                        , ( "four", 20.0 )
                                        , ( "one", 20.0 )
                                        , ( "three", 30.0 )
                                        , ( "two", 30.0 )
                                        ]
                                            |> Dict.fromList
                                            |> Histogrammar.BagOfFloatToString
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                ]
            , test "Binned - Simple" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Bin\",\"data\":{\"low\":-5.0,\"high\":5.0,\"entries\":123.0,\"name\":\"position [cm]\",\"values:type\":\"Count\",\"values\":[10.0,20.0,20.0,30.0,30.0],\"underflow:type\":\"Count\",\"underflow\":5.0,\"overflow:type\":\"Count\",\"overflow\":8.0,\"nanflow:type\":\"Count\",\"nanflow\":0.0}}"

                        expectedHistogram =
                            Histogrammar.Binned
                                { name = Just "position [cm]"
                                , low = -5.0
                                , high = 5.0
                                , entries = 123.0
                                , values =
                                    [ Histogrammar.Counted { entries = 10.0 }
                                    , Histogrammar.Counted { entries = 20.0 }
                                    , Histogrammar.Counted { entries = 20.0 }
                                    , Histogrammar.Counted { entries = 30.0 }
                                    , Histogrammar.Counted { entries = 30.0 }
                                    ]
                                , underflow = Histogrammar.Counted { entries = 5.0 }
                                , overflow = Histogrammar.Counted { entries = 8.0 }
                                , nanflow = Histogrammar.Counted { entries = 0.0 }
                                }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "Binned - Average" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"Bin\",\"data\":{\"low\":-5,\"high\":5,\"entries\":123,\"name\":\"position [cm]\",\"values:type\":\"Average\",\"values:name\":\"average time [s]\",\"values\":[{\"entries\":10,\"mean\":4.25},{\"entries\":20,\"mean\":16.21},{\"entries\":20,\"mean\":20.28},{\"entries\":30,\"mean\":16.19},{\"entries\":30,\"mean\":4.23}],\"underflow:type\":\"Count\",\"underflow\":5,\"overflow:type\":\"Count\",\"overflow\":8,\"nanflow:type\":\"Count\",\"nanflow\":0}}"

                        expectedHistogram =
                            Histogrammar.Binned
                                { name = Just "position [cm]"
                                , low = -5.0
                                , high = 5.0
                                , entries = 123.0
                                , values =
                                    [ Histogrammar.Averaged { name = Just "average time [s]", entries = 10.0, mean = 4.25 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 20.0, mean = 16.21 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 20.0, mean = 20.28 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 30.0, mean = 16.19 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 30.0, mean = 4.23 }
                                    ]
                                , underflow = Histogrammar.Counted { entries = 5.0 }
                                , overflow = Histogrammar.Counted { entries = 8.0 }
                                , nanflow = Histogrammar.Counted { entries = 0.0 }
                                }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            , test "SparselyBinned - Average" <|
                \() ->
                    let
                        jsonString =
                            "{\"version\":\"1.0\",\"type\":\"SparselyBin\",\"data\":{\"entries\":20000,\"origin\":0,\"nanflow:type\":\"Count\",\"bins:type\":\"Average\",\"nanflow\":0,\"binWidth\":0.1,\"bins\":{\"-6\":{\"entries\":674,\"mean\":0.77},\"-20\":{\"entries\":132,\"mean\":0.8},\"-23\":{\"entries\":80,\"mean\":0.71},\"-21\":{\"entries\":104,\"mean\":0.95},\"32\":{\"entries\":6,\"mean\":0.98},\"-9\":{\"entries\":540,\"mean\":0.79},\"-8\":{\"entries\":598,\"mean\":0.79}}}}"

                        expectedHistogram =
                            Histogrammar.Binned
                                { name = Nothing
                                , low = -5.0
                                , high = 5.0
                                , entries = 20000.0
                                , values =
                                    [ Histogrammar.Averaged { name = Just "average time [s]", entries = 10.0, mean = 4.25 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 20.0, mean = 16.21 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 20.0, mean = 20.28 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 30.0, mean = 16.19 }
                                    , Histogrammar.Averaged { name = Just "average time [s]", entries = 30.0, mean = 4.23 }
                                    ]
                                , underflow = Histogrammar.Counted { entries = 5.0 }
                                , overflow = Histogrammar.Counted { entries = 8.0 }
                                , nanflow = Histogrammar.Counted { entries = 0.0 }
                                }
                    in
                        expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
            ]
        ]
