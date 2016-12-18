module Histogrammar exposing (PastTenseHistogram(..), BagValues(..), histogramDecoder)

{-| This is the top-level module for an Elm library implementing the
[Histogrammar](http://histogrammar.org) specification.

# Past Tense Histogram Definitions
@docs PastTenseHistogram

-}

import Dict exposing (Dict)
import Array exposing (Array)
import Json.Decode as JD
import Histogrammar.Utils as Utils


type alias SpecificationVersion =
    ( Int, Int )


specificationVersion : SpecificationVersion
specificationVersion =
    ( 1, 0 )


type HistogramType
    = CountType
    | SumType
    | AverageType
    | DeviateType
    | MinimizeType
    | MaximizeType
    | BagType
    | BinType
    | SparselyBinType


{-| The various representations of the past-tense histogram types. These are
used for display and serialization.
-}
type PastTenseHistogram
    = Counted CountData
    | Summed SumData
    | Averaged AverageData
    | Deviated DeviateData
    | Minimized MinimizeData
    | Maximized MaximizeData
    | Bagged BagData
    | Binned BinData
    | SparselyBinned SparselyBinData


type alias CountData =
    { entries : Float
    }


type alias SumData =
    { name : Maybe String
    , entries : Float
    , sum : Float
    }


type alias AverageData =
    { name : Maybe String
    , entries : Float
    , mean : Float
    }


type alias DeviateData =
    { name : Maybe String
    , entries : Float
    , mean : Float
    , variance : Float
    }


type alias MinimizeData =
    { name : Maybe String
    , entries : Float
    , min : Float
    }


type alias MaximizeData =
    { name : Maybe String
    , entries : Float
    , max : Float
    }


type alias BagData =
    { name : Maybe String
    , entries : Float
    , values : BagValues
    }


type alias BinData =
    { name : Maybe String
    , entries : Float
    , low : Float
    , high : Float
    , values : List PastTenseHistogram
    , underflow : PastTenseHistogram
    , overflow : PastTenseHistogram
    , nanflow : PastTenseHistogram
    }


type BagRangeType
    = FloatToFloatType
    | FloatToFloatVectorType Int
    | FloatToStringType


type BagValues
    = BagOfFloatToFloat (Dict Float Float)
    | BagOfFloatToFloatVector Int (Dict (List Float) Float)
    | BagOfFloatToString (Dict String Float)


type alias SparselyBinData =
    { name : Maybe String
    , entries : Float
    , binWidth : Float
    , origin : Float
    , bins : Dict Int PastTenseHistogram
    , nanflow : PastTenseHistogram
    }


stringToHistogramType : String -> Result String HistogramType
stringToHistogramType histogramTypeString =
    case histogramTypeString of
        "Count" ->
            Ok CountType

        "Sum" ->
            Ok SumType

        "Average" ->
            Ok AverageType

        "Deviate" ->
            Ok DeviateType

        "Minimize" ->
            Ok MinimizeType

        "Maximize" ->
            Ok MaximizeType

        "Bag" ->
            Ok BagType

        "Bin" ->
            Ok BinType

        "SparselyBin" ->
            Ok SparselyBinType

        _ ->
            Err <| "Invalid histogram type " ++ toString histogramTypeString


histogramDecoder : JD.Decoder PastTenseHistogram
histogramDecoder =
    let
        version : JD.Decoder SpecificationVersion
        version =
            JD.field "version" JD.string
                |> JD.map (parseVersion >> Result.andThen verifyVersion)
                |> JD.andThen (Utils.unifyResult JD.fail JD.succeed)

        histogramType : JD.Decoder HistogramType
        histogramType =
            (JD.field "type" histogramTypeDecoder)
    in
        JD.andThen
            (\version ->
                JD.andThen (JD.field "data" << histogramDecoderForHistogramType) histogramType
            )
            version


histogramTypeDecoder : JD.Decoder HistogramType
histogramTypeDecoder =
    JD.string
        |> JD.map stringToHistogramType
        |> JD.andThen (Utils.unifyResult JD.fail JD.succeed)


verifyVersion : SpecificationVersion -> Result String SpecificationVersion
verifyVersion version =
    if version == specificationVersion then
        Ok version
    else
        Err <| "Incompatible version: " ++ toString version


parseVersion : String -> Result String SpecificationVersion
parseVersion versionString =
    case String.split "." versionString of
        [ major, minor ] ->
            Result.map2 (,)
                (String.toInt major)
                (String.toInt minor)
                |> Result.andThen Ok

        _ ->
            Err "Misshapen version format"


histogrammarFloatDecoder : JD.Decoder Float
histogrammarFloatDecoder =
    JD.oneOf
        [ JD.string |> JD.andThen histogrammarFloatDecoder_
        , JD.float
        ]


histogrammarFloatDecoder_ : String -> JD.Decoder Float
histogrammarFloatDecoder_ string =
    case string of
        "inf" ->
            JD.succeed <| 1 / 0

        "nan" ->
            JD.succeed <| 0 / 0

        "-inf" ->
            JD.succeed <| -1 / 0

        _ ->
            JD.fail <| "Invalid Float value: " ++ string


histogramDecoderForHistogramType : HistogramType -> JD.Decoder PastTenseHistogram
histogramDecoderForHistogramType histogramType =
    case histogramType of
        CountType ->
            JD.map Counted countDataDecoder

        SumType ->
            JD.map Summed sumDataDecoder

        AverageType ->
            JD.map Averaged averageDataDecoder

        DeviateType ->
            JD.map Deviated deviateDataDecoder

        MinimizeType ->
            JD.map Minimized minimizeDataDecoder

        MaximizeType ->
            JD.map Maximized maximizeDataDecoder

        BagType ->
            JD.map Bagged bagDataDecoder

        BinType ->
            JD.map Binned binDataDecoder

        SparselyBinType ->
            JD.map SparselyBinned sparselyBinDataDecoder


countDataDecoder : JD.Decoder CountData
countDataDecoder =
    JD.map CountData histogrammarFloatDecoder


sumDataDecoder : JD.Decoder SumData
sumDataDecoder =
    JD.map3 SumData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "sum" histogrammarFloatDecoder)


averageDataDecoder : JD.Decoder AverageData
averageDataDecoder =
    JD.map3 AverageData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "mean" histogrammarFloatDecoder)


deviateDataDecoder : JD.Decoder DeviateData
deviateDataDecoder =
    JD.map4 DeviateData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "mean" histogrammarFloatDecoder)
        (JD.field "variance" histogrammarFloatDecoder)


minimizeDataDecoder : JD.Decoder MinimizeData
minimizeDataDecoder =
    JD.map3 MinimizeData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "min" histogrammarFloatDecoder)


maximizeDataDecoder : JD.Decoder MaximizeData
maximizeDataDecoder =
    JD.map3 MaximizeData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "max" histogrammarFloatDecoder)


bagDataDecoder : JD.Decoder BagData
bagDataDecoder =
    (JD.field "range" JD.string)
        |> JD.andThen (parseRange >> Utils.unifyResult JD.fail decodeValuesFromRange)
        |> JD.map3 BagData
            (JD.maybe <| JD.field "name" JD.string)
            (JD.field "entries" histogrammarFloatDecoder)


parseRange : String -> Result String BagRangeType
parseRange rangeString =
    case String.uncons rangeString of
        Just ( 'N', "" ) ->
            Ok FloatToFloatType

        Just ( 'N', intString ) ->
            String.toInt intString |> Result.map FloatToFloatVectorType

        Just ( 'S', "" ) ->
            Ok FloatToStringType

        _ ->
            Err <| "Invalid range specifier: " ++ toString rangeString


decodeValuesFromRange : BagRangeType -> JD.Decoder BagValues
decodeValuesFromRange rangeType =
    case rangeType of
        FloatToFloatType ->
            JD.list
                (JD.map2 (,)
                    (JD.field "v" JD.float)
                    (JD.field "w" JD.float)
                )
                |> JD.field "values"
                |> JD.map (Dict.fromList >> BagOfFloatToFloat)

        FloatToFloatVectorType count ->
            JD.list
                (JD.map2 (,)
                    (JD.field "v" <| JD.andThen (verifyCount count) <| JD.list JD.float)
                    (JD.field "w" JD.float)
                )
                |> JD.field "values"
                |> JD.map (Dict.fromList >> BagOfFloatToFloatVector count)

        FloatToStringType ->
            (JD.map2 (,)
                (JD.field "v" JD.string)
                (JD.field "w" JD.float)
            )
                |> JD.list
                |> JD.field "values"
                |> JD.map (Dict.fromList >> BagOfFloatToString)


verifyCount : Int -> List a -> JD.Decoder (List a)
verifyCount count list =
    if count == List.length list then
        JD.succeed list
    else
        JD.fail <| "Invalid number of elements (expected " ++ toString count ++ ")"


binDataDecoder : JD.Decoder BinData
binDataDecoder =
    JD.map8 BinData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "low" JD.float)
        (JD.field "high" JD.float)
        (histogramTypeDecoder
            |> JD.field "values:type"
            |> JD.andThen
                (\histogramType ->
                    (JD.string
                        |> JD.field "values:name"
                        |> JD.maybe
                        |> JD.andThen
                            (\maybeName ->
                                (histogramDecoderForHistogramType histogramType
                                    |> JD.map (updateName maybeName)
                                    |> JD.list
                                    |> JD.field "values"
                                )
                            )
                    )
                )
        )
        (histogramTypeDecoder
            |> JD.field "underflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "underflow")
        )
        (histogramTypeDecoder
            |> JD.field "overflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "overflow")
        )
        (histogramTypeDecoder
            |> JD.field "nanflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "nanflow")
        )


updateName : Maybe String -> PastTenseHistogram -> PastTenseHistogram
updateName maybeName histogram =
    maybeName
        |> Maybe.map
            (\name ->
                case histogram of
                    Counted data ->
                        Counted data

                    Summed histogramData ->
                        Summed { histogramData | name = Just name }

                    Averaged histogramData ->
                        Averaged { histogramData | name = Just name }

                    Deviated histogramData ->
                        Deviated { histogramData | name = Just name }

                    Minimized histogramData ->
                        Minimized { histogramData | name = Just name }

                    Maximized histogramData ->
                        Maximized { histogramData | name = Just name }

                    Bagged histogramData ->
                        Bagged { histogramData | name = Just name }

                    Binned histogramData ->
                        Binned { histogramData | name = Just name }

                    SparselyBinned histogramData ->
                        SparselyBinned { histogramData | name = Just name }
            )
        |> Maybe.withDefault histogram


sparselyBinDataDecoder : JD.Decoder SparselyBinData
sparselyBinDataDecoder =
    JD.map6 SparselyBinData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "binWidth" JD.float)
        (JD.field "origin" JD.float)
        (histogramTypeDecoder
            |> JD.field "bins:type"
            |> JD.andThen
                (\histogramType ->
                    JD.field "values:name" JD.string
                        |> JD.maybe
                        |> JD.andThen
                            (\maybeName ->
                                histogramType
                                    |> histogramDecoderForHistogramType
                                    |> JD.dict
                                    |> JD.andThen
                                        (stringToIntKeyTransformingDictDecoder <|
                                            JD.succeed Dict.empty
                                        )
                                    |> JD.andThen
                                        (updateNameTransformingDictDecoder maybeName <|
                                            JD.succeed Dict.empty
                                        )
                            )
                )
        )
        (histogramTypeDecoder
            |> JD.field "nanflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "nanflow")
        )


stringToIntKeyTransformingDictDecoder :
    JD.Decoder (Dict Int PastTenseHistogram)
    -> Dict String PastTenseHistogram
    -> JD.Decoder (Dict Int PastTenseHistogram)
stringToIntKeyTransformingDictDecoder =
    Dict.foldl
        (\key value json ->
            case String.toInt key of
                Ok int ->
                    JD.map (Dict.insert int value) json

                Err error ->
                    JD.fail error
        )


updateNameTransformingDictDecoder :
    Maybe String
    -> JD.Decoder (Dict Int PastTenseHistogram)
    -> Dict Int PastTenseHistogram
    -> JD.Decoder (Dict Int PastTenseHistogram)
updateNameTransformingDictDecoder maybeName =
    Dict.foldl
        (\key value ->
            JD.map (Dict.update key (Maybe.map <| updateName maybeName))
        )
