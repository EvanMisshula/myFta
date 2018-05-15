module MyFta exposing (..)

{-| This module shows how to build a simple bar chart.
-}

--import Date exposing (Date)
--import Date.Extra as Date
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
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
                   , ciLower: List (Strategy, Float)
                   , ciUpper: List (Strategy, Float)
                   , stratCount : List (Strategy, Int)
                   }
               
model : Model
model = { nRange = [ ( Uncalled, 23.2)
                   , ( Notified, 15.5)
                   ] 
        , ciLower = [ ( Uncalled, 19.93253)
                    , ( Notified, 14.03265)
                    ] 
        , ciUpper = [ ( Uncalled, 26.67034)
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
            [ x <| toString <| Scale.convert (Scale.toRenderable xScale) myStrategy
            , y <| toString <| Scale.convert yScale value - 5
            , textAnchor "middle"
            ]
            [ text <| toString value ]
        ]


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
            [ yAxis ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
            List.map (column (xScale model.nRange)) model.nRange
        ]

main : Svg msg
main =
    view model
