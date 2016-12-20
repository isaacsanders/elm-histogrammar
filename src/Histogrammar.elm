module Histogrammar
    exposing
        ( PastTenseHistogram(..)
        , histogramDecoder
        , BagValues(..)
        , HistogramType(..)
        , SparselyBinData
        , sparselyBinDataDecoder
        , sparselyBinsDecoder
        )

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
    | CentrallyBinType
    | IrregularlyBinType
    | CategorizeType
    | FractionType


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
    | CentrallyBinned CentrallyBinData
    | IrregularlyBinned IrregularlyBinData
    | Categorized CategorizeData
    | Fractioned FractionData


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


type alias CentrallyBinData =
    { name : Maybe String
    , entries : Float
    , bins : Dict Float PastTenseHistogram
    , nanflow : PastTenseHistogram
    }


type alias IrregularlyBinData =
    { name : Maybe String
    , entries : Float
    , bins : Dict Float PastTenseHistogram
    , nanflow : PastTenseHistogram
    }


type alias CategorizeData =
    { name : Maybe String
    , entries : Float
    , bins : Dict String PastTenseHistogram
    }


type alias FractionData =
    { name : Maybe String
    , entries : Float
    , numerator : PastTenseHistogram
    , denominator : PastTenseHistogram
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

        "CentrallyBin" ->
            Ok CentrallyBinType

        "IrregularlyBin" ->
            Ok IrregularlyBinType

        "Categorize" ->
            Ok CategorizeType

        "Fraction" ->
            Ok FractionType

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

        CentrallyBinType ->
            JD.map CentrallyBinned centrallyBinDataDecoder

        IrregularlyBinType ->
            JD.map IrregularlyBinned irregularlyBinDataDecoder

        CategorizeType ->
            JD.map Categorized categorizeDataDecoder

        FractionType ->
            JD.map Fractioned fractionDataDecoder


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
        (subAggregatorDecoder "values:type" "values:name" <|
            \histogramType maybeName ->
                (histogramDecoderForHistogramType histogramType
                    |> JD.map (updateName maybeName)
                    |> JD.list
                    |> JD.field "values"
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

                    CentrallyBinned histogramData ->
                        CentrallyBinned { histogramData | name = Just name }

                    IrregularlyBinned histogramData ->
                        IrregularlyBinned { histogramData | name = Just name }

                    Categorized histogramData ->
                        Categorized { histogramData | name = Just name }

                    Fractioned histogramData ->
                        Fractioned { histogramData | name = Just name }
            )
        |> Maybe.withDefault histogram


sparselyBinDataDecoder : JD.Decoder SparselyBinData
sparselyBinDataDecoder =
    JD.map6 SparselyBinData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (JD.field "binWidth" JD.float)
        (JD.field "origin" JD.float)
        (subAggregatorDecoder
            "bins:type"
            "values:name"
         <|
            \histogramType ->
                sparselyBinsDecoder histogramType >> JD.field "bins"
        )
        (histogramTypeDecoder
            |> JD.field "nanflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "nanflow")
        )


sparselyBinsDecoder : HistogramType -> Maybe String -> JD.Decoder (Dict Int PastTenseHistogram)
sparselyBinsDecoder histogramType maybeName =
    histogramType
        |> histogramDecoderForHistogramType
        |> JD.dict
        |> JD.andThen
            (parseKeyTransformingDictDecoder String.toInt)
        |> JD.andThen
            (updateNameTransformingDictDecoder maybeName)


centrallyBinDataDecoder : JD.Decoder CentrallyBinData
centrallyBinDataDecoder =
    JD.map4 CentrallyBinData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (subAggregatorDecoder
            "bins:type"
            "bins:name"
            (\histogramType maybeName ->
                centrallyBinsDecoder histogramType maybeName
                    |> JD.field "bins"
            )
        )
        (histogramTypeDecoder
            |> JD.field "nanflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "nanflow")
        )


centrallyBinsDecoder : HistogramType -> Maybe String -> JD.Decoder (Dict Float PastTenseHistogram)
centrallyBinsDecoder histogramType maybeName =
    let
        binDecoder : JD.Decoder PastTenseHistogram
        binDecoder =
            histogramType
                |> histogramDecoderForHistogramType
                |> JD.field "data"
    in
        JD.map2 (,) (JD.field "center" JD.float) binDecoder
            |> JD.list
            |> JD.map Dict.fromList
            |> JD.andThen
                (updateNameTransformingDictDecoder maybeName)


irregularlyBinDataDecoder : JD.Decoder IrregularlyBinData
irregularlyBinDataDecoder =
    JD.map4 IrregularlyBinData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (subAggregatorDecoder
            "bins:type"
            "bins:name"
            (\histogramType maybeName ->
                irregularlyBinsDecoder histogramType maybeName
                    |> JD.field "bins"
            )
        )
        (histogramTypeDecoder
            |> JD.field "nanflow:type"
            |> JD.andThen
                (histogramDecoderForHistogramType >> JD.field "nanflow")
        )


irregularlyBinsDecoder : HistogramType -> Maybe String -> JD.Decoder (Dict Float PastTenseHistogram)
irregularlyBinsDecoder histogramType maybeName =
    let
        binDecoder : JD.Decoder PastTenseHistogram
        binDecoder =
            histogramType
                |> histogramDecoderForHistogramType
                |> JD.field "data"
    in
        JD.map2 (,) (JD.field "atleast" histogrammarFloatDecoder) binDecoder
            |> JD.list
            |> JD.map Dict.fromList
            |> JD.andThen
                (updateNameTransformingDictDecoder maybeName)


categorizeDataDecoder : JD.Decoder CategorizeData
categorizeDataDecoder =
    JD.map3 CategorizeData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (subAggregatorDecoder
            "bins:type"
            "bins:name"
            (\histogramType maybeName ->
                categoriesDecoder histogramType maybeName
                    |> JD.field "bins"
            )
        )


categoriesDecoder : HistogramType -> Maybe String -> JD.Decoder (Dict String PastTenseHistogram)
categoriesDecoder histogramType maybeName =
    let
        binDecoder : JD.Decoder PastTenseHistogram
        binDecoder =
            histogramType
                |> histogramDecoderForHistogramType
    in
        JD.dict binDecoder
            |> JD.andThen
                (updateNameTransformingDictDecoder maybeName)


fractionDataDecoder : JD.Decoder FractionData
fractionDataDecoder =
    JD.map4 FractionData
        (JD.maybe <| JD.field "name" JD.string)
        (JD.field "entries" histogrammarFloatDecoder)
        (subAggregatorDecoder
            "sub:type"
            "sub:name"
            (\histogramType maybeName ->
                histogramDecoderForHistogramType histogramType
                    |> JD.map (updateName maybeName)
                    |> JD.field "numerator"
            )
        )
        (subAggregatorDecoder
            "sub:type"
            "sub:name"
            (\histogramType maybeName ->
                histogramDecoderForHistogramType histogramType
                    |> JD.map (updateName maybeName)
                    |> JD.field "denominator"
            )
        )


subAggregatorDecoder : String -> String -> (HistogramType -> Maybe String -> JD.Decoder a) -> JD.Decoder a
subAggregatorDecoder histogramTypeField nameField decoderFn =
    JD.field histogramTypeField histogramTypeDecoder
        |> JD.andThen
            (\histogramType ->
                JD.field nameField JD.string
                    |> JD.maybe
                    |> JD.andThen (decoderFn histogramType)
            )


parseKeyTransformingDictDecoder :
    (String -> Result String comparable)
    -> Dict String PastTenseHistogram
    -> JD.Decoder (Dict comparable PastTenseHistogram)
parseKeyTransformingDictDecoder keyMapper =
    Dict.empty
        |> JD.succeed
        |> Dict.foldl
            (\key value json ->
                case keyMapper key of
                    Ok mappedKey ->
                        JD.map (Dict.insert mappedKey value) json

                    Err error ->
                        JD.fail error
            )


updateNameTransformingDictDecoder :
    Maybe String
    -> Dict comparable PastTenseHistogram
    -> JD.Decoder (Dict comparable PastTenseHistogram)
updateNameTransformingDictDecoder maybeName =
    Dict.empty
        |> JD.succeed
        |> Dict.foldl
            (\key value json ->
                JD.map
                    (Dict.insert key (updateName maybeName value))
                    json
            )
