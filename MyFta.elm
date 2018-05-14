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

type alias Model = { data : List ( Int, Float )
                   , xLabels : List ( Int, String ) }

model : Model
model = { data = [ ( 1, 23.2 )
                 , ( 2, 15.5 )
                 ]
        ,  xLabels = [ ( 1, "no-notification" )
                     , ( 2,  "notification" )
                     ]
        }

xScale : List ( Int, String ) -> BandScale Int
xScale model =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 30 ) ( h - 2 * padding, 0 )


-- xAxis : List ( Int, Float ) -> Svg msg
-- xAxis model =
--     Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (toString ) } (Scale.toRenderable (xScale model))

--xAxis : List ( Int, String ) -> Svg msg
xAxis xLabels =
    Axis.axis { defaultOptions | orientation = Axis.Bottom } (Scale.toRenderable (xScale xLabels))
        

yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 6 } yScale


column : BandScale Int -> ( Int, Float ) -> Svg msg
column xScale ( myInt, value ) =
    g [ class "column" ]
        [ rect
            [ x <| toString <| Scale.convert xScale myInt
            , y <| toString <| Scale.convert yScale value
            , width <| toString <| Scale.bandwidth xScale
            , height <| toString <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| toString <| Scale.convert (Scale.toRenderable xScale) myInt
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
            [ xAxis model.xLabels ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
            List.map (column (xScale model.xLabels)) model.data
        ]

main : Svg msg
main =
    view model
