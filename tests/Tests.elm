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
            , describe "Binned"
                [ test "Simple" <|
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
                , test "Average" <|
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
                ]
            , describe "SparselyBinned"
                [ test "Count" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"SparselyBin\",\"data\":{\"binWidth\":2,\"entries\":123,\"bins:type\":\"Count\",\"bins\":{\"-999\":5,\"-4\":10,\"-2\":20,\"0\":20,\"2\":30,\"4\":30,\"12345\":8},\"nanflow:type\":\"Count\",\"nanflow\":0,\"origin\":0,\"name\":\"myfunc\"}}"

                            expectedHistogram =
                                Histogrammar.SparselyBinned
                                    { name = Just "myfunc"
                                    , entries = 123.0
                                    , binWidth = 2.0
                                    , origin = 0.0
                                    , bins =
                                        Dict.fromList
                                            [ ( -999, Histogrammar.Counted { entries = 5.0 } )
                                            , ( -4, Histogrammar.Counted { entries = 10.0 } )
                                            , ( -2, Histogrammar.Counted { entries = 20.0 } )
                                            , ( 0, Histogrammar.Counted { entries = 20.0 } )
                                            , ( 2, Histogrammar.Counted { entries = 30.0 } )
                                            , ( 4, Histogrammar.Counted { entries = 30.0 } )
                                            , ( 12345, Histogrammar.Counted { entries = 8.0 } )
                                            ]
                                    , nanflow = Histogrammar.Counted { entries = 0.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "Average" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"SparselyBin\",\"data\":{\"origin\":0,\"entries\":20000,\"nanflow:type\":\"Count\",\"bins:type\":\"Average\",\"nanflow\":0,\"binWidth\":0.1,\"bins\":{\"-6\":{\"entries\":674,\"mean\":0.77},\"-20\":{\"entries\":132,\"mean\":0.8},\"-23\":{\"entries\":80,\"mean\":0.71},\"-21\":{\"entries\":104,\"mean\":0.95},\"32\":{\"entries\":6,\"mean\":0.98},\"-9\":{\"entries\":540,\"mean\":0.79},\"-8\":{\"entries\":598,\"mean\":0.79}}}}"

                            expectedHistogram =
                                Histogrammar.SparselyBinned
                                    { name = Nothing
                                    , entries = 20000.0
                                    , binWidth = 0.1
                                    , origin = 0.0
                                    , bins =
                                        Dict.fromList
                                            [ ( -23, Histogrammar.Averaged { name = Nothing, entries = 80.0, mean = 0.71 } )
                                            , ( -21, Histogrammar.Averaged { name = Nothing, entries = 104.0, mean = 0.95 } )
                                            , ( -20, Histogrammar.Averaged { name = Nothing, entries = 132.0, mean = 0.8 } )
                                            , ( -9, Histogrammar.Averaged { name = Nothing, entries = 540.0, mean = 0.79 } )
                                            , ( -8, Histogrammar.Averaged { name = Nothing, entries = 598.0, mean = 0.79 } )
                                            , ( -6, Histogrammar.Averaged { name = Nothing, entries = 674.0, mean = 0.77 } )
                                            , ( 32, Histogrammar.Averaged { name = Nothing, entries = 6.0, mean = 0.98 } )
                                            ]
                                    , nanflow = Histogrammar.Counted { entries = 0.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                ]
            , describe "CentrallyBinned"
                [ test "Count" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"CentrallyBin\",\"data\":{\"entries\":123,\"bins:type\":\"Count\",\"bins\":[{\"center\":-999,\"data\":5},{\"center\":-4,\"data\":10},{\"center\":-2,\"data\":20},{\"center\":0,\"data\":20},{\"center\":2,\"data\":30},{\"center\":4,\"data\":30},{\"center\":12345,\"data\":8}],\"nanflow:type\":\"Count\",\"nanflow\":0,\"name\":\"myfunc\"}}"

                            expectedHistogram =
                                Histogrammar.CentrallyBinned
                                    { name = Just "myfunc"
                                    , entries = 123.0
                                    , bins =
                                        Dict.fromList
                                            [ ( -999, Histogrammar.Counted { entries = 5.0 } )
                                            , ( -4, Histogrammar.Counted { entries = 10.0 } )
                                            , ( -2, Histogrammar.Counted { entries = 20.0 } )
                                            , ( 0, Histogrammar.Counted { entries = 20.0 } )
                                            , ( 2, Histogrammar.Counted { entries = 30.0 } )
                                            , ( 4, Histogrammar.Counted { entries = 30.0 } )
                                            , ( 12345, Histogrammar.Counted { entries = 8.0 } )
                                            ]
                                    , nanflow = Histogrammar.Counted { entries = 0.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "Average" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"CentrallyBin\",\"data\":{\"entries\":123,\"bins:type\":\"Average\",\"bins\":[{\"center\":-999,\"data\":{\"entries\":5,\"mean\":-1}},{\"center\":-4,\"data\":{\"entries\":10,\"mean\":4.25}},{\"center\":-2,\"data\":{\"entries\":20,\"mean\":16.21}},{\"center\":0,\"data\":{\"entries\":20,\"mean\":20.28}},{\"center\":2,\"data\":{\"entries\":20,\"mean\":16.19}},{\"center\":4,\"data\":{\"entries\":30,\"mean\":4.23}},{\"center\":12345,\"data\":{\"entries\":8,\"mean\":-1}}],\"nanflow:type\":\"Count\",\"nanflow\":0,\"name\":\"myfunc\",\"bins:name\":\"myfunc2\"}}"

                            expectedHistogram =
                                Histogrammar.CentrallyBinned
                                    { name = Just "myfunc"
                                    , entries = 123.0
                                    , bins =
                                        Dict.fromList
                                            [ ( -999.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 5.0, mean = -1.0 } )
                                            , ( -4.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 10.0, mean = 4.25 } )
                                            , ( -2.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 20.0, mean = 16.21 } )
                                            , ( 0.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 20.0, mean = 20.28 } )
                                            , ( 2.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 20.0, mean = 16.19 } )
                                            , ( 4.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 30.0, mean = 4.23 } )
                                            , ( 12345.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 8.0, mean = -1.0 } )
                                            ]
                                    , nanflow = Histogrammar.Counted { entries = 0.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                ]
            , describe "IrregularlyBinned"
                [ test "Count" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"IrregularlyBin\",\"data\":{\"entries\":123,\"bins:type\":\"Count\",\"bins\":[{\"atleast\":\"-inf\",\"data\":23},{\"atleast\":1,\"data\":20},{\"atleast\":2,\"data\":20},{\"atleast\":3,\"data\":30},{\"atleast\":4,\"data\":30}],\"nanflow:type\":\"Count\",\"nanflow\":0,\"name\":\"myfunc\"}}"

                            expectedHistogram =
                                Histogrammar.IrregularlyBinned
                                    { name = Just "myfunc"
                                    , entries = 123.0
                                    , bins =
                                        Dict.fromList
                                            [ ( -1 / 0, Histogrammar.Counted { entries = 23.0 } )
                                            , ( 1.0, Histogrammar.Counted { entries = 20.0 } )
                                            , ( 2.0, Histogrammar.Counted { entries = 20.0 } )
                                            , ( 3.0, Histogrammar.Counted { entries = 30.0 } )
                                            , ( 4.0, Histogrammar.Counted { entries = 30.0 } )
                                            ]
                                    , nanflow = Histogrammar.Counted { entries = 0.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "Average" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"IrregularlyBin\",\"data\":{\"entries\":123,\"bins:type\":\"Average\",\"bins\":[{\"atleast\":\"-inf\",\"data\":{\"entries\":23,\"mean\":3.14}},{\"atleast\":1,\"data\":{\"entries\":20,\"mean\":2.28}},{\"atleast\":2,\"data\":{\"entries\":20,\"mean\":1.16}},{\"atleast\":3,\"data\":{\"entries\":30,\"mean\":8.9}},{\"atleast\":4,\"data\":{\"entries\":30,\"mean\":22.7}}],\"nanflow:type\":\"Count\",\"nanflow\":0,\"name\":\"myfunc\",\"bins:name\":\"myfunc2\"}}"

                            expectedHistogram =
                                Histogrammar.IrregularlyBinned
                                    { name = Just "myfunc"
                                    , entries = 123.0
                                    , bins =
                                        Dict.fromList
                                            [ ( -1 / 0, Histogrammar.Averaged { name = Just "myfunc2", entries = 23.0, mean = 3.14 } )
                                            , ( 1.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 20.0, mean = 2.28 } )
                                            , ( 2.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 20.0, mean = 1.16 } )
                                            , ( 3.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 30.0, mean = 8.9 } )
                                            , ( 4.0, Histogrammar.Averaged { name = Just "myfunc2", entries = 30.0, mean = 22.7 } )
                                            ]
                                    , nanflow = Histogrammar.Counted { entries = 0.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                ]
            , describe "Categorized"
                [ test "Count" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"Categorize\",\"data\":{\"entries\":123,\"bins:type\":\"Count\",\"bins\":{\"one\":23,\"two\":20,\"three\":20,\"four\":30,\"five\":30},\"name\":\"myfunc\"}}"

                            expectedHistogram =
                                Histogrammar.Categorized
                                    { name = Just "myfunc"
                                    , entries = 123.0
                                    , bins =
                                        Dict.fromList
                                            [ ( "one", Histogrammar.Counted { entries = 23.0 } )
                                            , ( "two", Histogrammar.Counted { entries = 20.0 } )
                                            , ( "three", Histogrammar.Counted { entries = 20.0 } )
                                            , ( "four", Histogrammar.Counted { entries = 30.0 } )
                                            , ( "five", Histogrammar.Counted { entries = 30.0 } )
                                            ]
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "Average" <|
                    \() ->
                        let
                            jsonString =
                                "{\"data\":{\"bins:type\":\"Average\",\"bins\":{\"one\":{\"entries\":4706,\"mean\":-0.91},\"three\":{\"entries\":112,\"mean\":-2.78},\"TWO\":{\"entries\":1180,\"mean\":1.84},\"FOUR\":{\"entries\":10,\"mean\":3.92},\"THREE\":{\"entries\":146,\"mean\":2.78}},\"entries\":20000},\"version\":\"1.0\",\"type\":\"Categorize\"}"

                            expectedHistogram =
                                Histogrammar.Categorized
                                    { name = Nothing
                                    , entries = 20000.0
                                    , bins =
                                        Dict.fromList
                                            [ ( "FOUR", Histogrammar.Averaged { name = Nothing, entries = 10.0, mean = 3.92 } )
                                            , ( "THREE", Histogrammar.Averaged { name = Nothing, entries = 146.0, mean = 2.78 } )
                                            , ( "TWO", Histogrammar.Averaged { name = Nothing, entries = 1180.0, mean = 1.84 } )
                                            , ( "one", Histogrammar.Averaged { name = Nothing, entries = 4706.0, mean = -0.91 } )
                                            , ( "three", Histogrammar.Averaged { name = Nothing, entries = 112.0, mean = -2.78 } )
                                            ]
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                ]
            , describe "Fractioned"
                [ test "Count" <|
                    \() ->
                        let
                            jsonString =
                                "{\"data\":{\"sub:type\":\"Count\",\"denominator\":10000,\"numerator\":1317,\"entries\":10000},\"version\":\"1.0\",\"type\":\"Fraction\"}"

                            expectedHistogram =
                                Histogrammar.Fractioned
                                    { name = Nothing
                                    , entries = 10000.0
                                    , numerator = Histogrammar.Counted { entries = 1317.0 }
                                    , denominator = Histogrammar.Counted { entries = 10000.0 }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                , test "Bin(Count)" <|
                    \() ->
                        let
                            jsonString =
                                "{\"version\":\"1.0\",\"type\":\"Fraction\",\"data\":{\"entries\":123,\"name\":\"trigger\",\"sub:name\":\"energy [GeV]\",\"sub:type\":\"Bin\",\"numerator\":{\"low\":-5,\"high\":5,\"entries\":98,\"values:type\":\"Count\",\"values\":[2,15,18,25,30],\"underflow:type\":\"Count\",\"underflow\":0,\"overflow:type\":\"Count\",\"overflow\":8,\"nanflow:type\":\"Count\",\"nanflow\":0},\"denominator\":{\"low\":-5,\"high\":5,\"entries\":123,\"values:type\":\"Count\",\"values\":[10,20,20,30,30],\"underflow:type\":\"Count\",\"underflow\":5,\"overflow:type\":\"Count\",\"overflow\":8,\"nanflow:type\":\"Count\",\"nanflow\":0}}}"

                            expectedHistogram =
                                Histogrammar.Fractioned
                                    { name = Just "trigger"
                                    , entries = 123.0
                                    , numerator =
                                        Histogrammar.Binned
                                            { name = Just "energy [GeV]"
                                            , entries = 98.0
                                            , low = -5
                                            , high = 5.0
                                            , values =
                                                [ Histogrammar.Counted { entries = 2 }
                                                , Histogrammar.Counted { entries = 15 }
                                                , Histogrammar.Counted { entries = 18 }
                                                , Histogrammar.Counted { entries = 25 }
                                                , Histogrammar.Counted { entries = 30 }
                                                ]
                                            , underflow = Histogrammar.Counted { entries = 0 }
                                            , overflow = Histogrammar.Counted { entries = 8 }
                                            , nanflow = Histogrammar.Counted { entries = 0 }
                                            }
                                    , denominator =
                                        Histogrammar.Binned
                                            { name = Just "energy [GeV]"
                                            , entries = 123
                                            , low = -5
                                            , high = 5
                                            , values =
                                                [ Histogrammar.Counted { entries = 10 }
                                                , Histogrammar.Counted { entries = 20 }
                                                , Histogrammar.Counted { entries = 20 }
                                                , Histogrammar.Counted { entries = 30 }
                                                , Histogrammar.Counted { entries = 30 }
                                                ]
                                            , underflow = Histogrammar.Counted { entries = 5 }
                                            , overflow = Histogrammar.Counted { entries = 8 }
                                            , nanflow = Histogrammar.Counted { entries = 0 }
                                            }
                                    }
                        in
                            expectDecodedEquals Histogrammar.histogramDecoder jsonString expectedHistogram
                ]
            ]
        ]
