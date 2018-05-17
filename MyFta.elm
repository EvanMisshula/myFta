module MyFta exposing (..)

{-| This module shows how to build a simple bar chart.
-}

--import Date exposing (Date)
--import Date.Extra as Date
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape
-- import SampleData exposing (timeSeries)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30

type alias Model = { nRange : List (Strategy, Float)
                   , ciUncalled : List (Strategy, Float)  
                   , ciNotified : List (Strategy, Float)
                   , stratCount : List (Strategy, Int)
                   }

model : Model
model = { nRange = [ ( Uncalled, 23.2)
                   , ( Notified, 15.5)
                   ] 
        , ciUncalled = [ ( Uncalled, 19.93253)
                       , ( Uncalled, 26.67034)
                       ] 
        , ciNotified = [ ( Notified, 14.03265)
                       , ( Notified, 17.02423)
                    ]
        , stratCount = [ ( Uncalled, 630)
                       , ( Notified, 2312)
                       ]
        }
type Strategy = Uncalled
              | Notified

fromStrategyToString : Strategy -> String
fromStrategyToString myStrat =
    case myStrat of
        Uncalled -> "no-notification"
        Notified -> "notification"
        
xScale : List (Strategy, Float ) -> BandScale Strategy
xScale model =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


        
yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 30 ) ( h - 2 * padding, 0 )

xAxis : List ( Strategy, Float ) -> Svg msg
xAxis nRange =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (fromStrategyToString) } (Scale.toRenderable (xScale nRange))
        

yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 6 } yScale


column : BandScale Strategy -> ( Strategy, Float ) -> Svg msg
column xScale ( myStrategy, value ) =
    g [ class "column" ]
        [ rect
            [ x <| toString <| Scale.convert xScale myStrategy
            , y <| toString <| Scale.convert yScale value
            , width <| toString <| Scale.bandwidth xScale
            , height <| toString <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| toString <| ((Scale.convert (Scale.toRenderable xScale) myStrategy) + 20.0)
            , y <| toString <| Scale.convert yScale value - 5
            , textAnchor "right"
            ]
            [ text <| toString value ]
        ]

lineGenerator : List (Strategy, Float) -> (Strategy, Float) -> Maybe (Float, Float)
lineGenerator nRange (x, y) =
    Just (Scale.convert (xScale nRange) x, Scale.convert yScale y)
        
myCI : List (Strategy, Float) -> List (Strategy, Float) -> String
myCI nRange data =
    List.map (lineGenerator nRange) data
        |> Shape.line Shape.linearCurve

makeHline: List (Strategy, Float) -> (Strategy, Float) ->  String
makeHline nRange myPoint =
    let
        x = Tuple.first myPoint
        y = Tuple.second myPoint
        myX1 = (Scale.convert (xScale nRange) x) - 25.0
        myX2 = (Scale.convert (xScale nRange) x) + 25.0
        myY  = Scale.convert yScale y
    in
        [Just (myX1,myY), Just (myX2,myY)] |> Shape.line Shape.linearCurve

myFirst : List (Strategy, Float) -> (Strategy, Float)
myFirst myCi =
    let
        mF = List.head myCi
    in
        case mF of
            Just mF -> mF
            Nothing -> (Uncalled, 0.0)

mySec : List (Strategy, Float) -> (Strategy, Float)
mySec myCi =
    let
        mS = List.head (List.reverse myCi)
    in
        case mS of
            Just mS -> mS
            Nothing -> (Uncalled, 0.0)


                       
bandOffset : Model  -> String
bandOffset model  =
    toString (0.5 * (Scale.bandwidth (xScale model.nRange)) + padding)
    
view : Model -> Svg msg
view model =
    svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ Svg.style [] [ text """
            .column rect { fill: rgba(70, 130, 180, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(70, 130, 180); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis model.nRange ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis, text_ [fontFamily "sans-serif", fontSize "15", x "5", y "5" ] [ text "% Fta"]]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
         List.map (column (xScale model.nRange)) model.nRange

        , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), class "ci" ]
            [Svg.path [d (makeHline model.nRange (myFirst model.ciUncalled)), stroke "black", strokeWidth "2px", fill "none" ]
                 []
            ]
        , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), class "ci" ]
            [Svg.path [d (myCI model.nRange model.ciUncalled), stroke "black", strokeWidth "2px", fill "none" ]
             []
            ]
        , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), class "ci" ]
            [Svg.path [d (makeHline model.nRange (mySec model.ciUncalled)), stroke "black", strokeWidth "2px", fill "none" ]
             []
            ]
        , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), class "ci" ]
            [Svg.path [d (makeHline model.nRange (myFirst model.ciNotified)), stroke "black", strokeWidth "2px", fill "none" ]
                 []
            ]
        , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), class "ci" ] [Svg.path [d (myCI model.nRange model.ciNotified), stroke "black", strokeWidth "2px", fill "none" ][]]
        , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), class "ci" ]
            [Svg.path [d (makeHline model.nRange (mySec model.ciNotified)), stroke "black", strokeWidth "2px", fill "none" ]
                 []
            ]
        ]

main : Svg msg
main =
    view model
