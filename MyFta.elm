module MyFta exposing (..)

{-| This module shows how to build a simple bar chart.
-}

--import Date exposing (Date)
--import Date.Extra as Date
import Ease exposing (..)
import AnimationFrame as Af exposing (times)
import Animation as A exposing (from, to, ease, animation)
import Time as T exposing (second)
import Katex exposing (Latex, human, inline, display, print)
--import Color exposing (..)
import Html as Html exposing (Html, div, p, fieldset, label)
import Html.Attributes  as Hatt exposing (..)
import Html.Events as Hevent exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as Satt exposing (..)
import Round as Erd exposing (round)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
-- import SampleData exposing (timeSeries)


w : Float
w =
    950


h : Float
h =
    500


padding : Float
padding =
    45

type alias Model = { nRange : List (Strategy, Float)
                   , nNull : List (Strategy, Float)
                   , nResearch : List (Strategy, Float)
                   , nValues : List (Strategy, Float)
                   , ciUncalled : List (Strategy, Float)  
                   , ciNotified : List (Strategy, Float)
                   , stratCount : List (Strategy, Int)
                   , ciOpacity : Float
                   , currentTick : T.Time
                   , opacityAnimation : Maybe A.Animation
                   , uncalledAnimation : Maybe A.Animation
                   , notifiedAnimation : Maybe A.Animation
                   , tableAnimation : Maybe A.Animation
                   , startOpacity : Float
                   , endOpacity : Float
                   , startUncalled : Float
                   , endUncalled : Float
                   , startNotified : Float
                   , endNotified : Float
                   , thisRegime : Regime
                   , myResults : List ExpResult
                   , tableWidth : Float
                   , showTest : Bool
                   , chiSq : Float
                   , pValue : Float
                   }

initialModel : Model
initialModel = { nRange = [ ( Uncalled, 23.2)
                          , ( Notified, 15.5)
                          ]
               , nNull = [ ( Uncalled, 17.1312)
                         , ( Notified, 17.1312)
                         ]
               , nResearch = [ ( Uncalled, 23.2)
                             , ( Notified, 15.5)
                             ]
               , nValues = [ ( Uncalled, 17.1312)
                           , ( Notified, 17.1312)
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
               , ciOpacity = 0.0
               , currentTick = 0
               , opacityAnimation = Nothing
               , uncalledAnimation = Nothing
               , notifiedAnimation = Nothing
               , tableAnimation = Nothing
               , startOpacity = 0.0
               , endOpacity = 1.0
               , startUncalled = 17.1312
               , endUncalled = 23.2
               , startNotified = 17.1312
               , endNotified = 15.5
               , thisRegime = Theory
               , myResults =
                     [ (Uncalled, NoWarrant,484)
                     , (Uncalled, WarrantIss, 146)
                     , (Notified, NoWarrant, 1954)
                     , (Notified, WarrantIss, 358)
                     ]
               , tableWidth = 0.0
               , showTest = False
               , chiSq = 20.086
               , pValue = 3.701531 * 10^(-6)

               }



         
selectCases : Strategy -> DefOutcome -> Model -> Int
selectCases strategy defOutcome model =
    let
        tupleEq strategy defoutcome =
            \(a,b,_) -> (a == strategy) && (b == defOutcome)
        myMaybeElement = List.head (List.filter (tupleEq strategy defOutcome)  model.myResults)
    in
        case myMaybeElement of
            Nothing -> 0
            Just (_, _, c) -> c
    

type Strategy = Uncalled
              | Notified

type Msg = ToggleCi
         | CurrentTick T.Time
         | SwitchTo Regime
         | ToggleCMatrix
         | ToggleTest 
             
type Regime = Theory
    | Reality

type DefOutcome = WarrantIss
                | NoWarrant

type alias ExpResult = (Strategy, DefOutcome, Int)
    
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
    Axis.axis { defaultOptions
                  | orientation = Axis.Bottom
                  , tickFormat = Just (fromStrategyToString)
                  , tickSizeInner = 15
                  , tickSizeOuter = 6
              } (Scale.toRenderable (xScale nRange))
        

yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 6 } yScale

translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")

column : BandScale Strategy -> ( Strategy, Float ) -> Svg msg
column xScale ( myStrategy, value ) =
    g [ Satt.class "column" ]
        [ rect
            [ x <| toString <| Scale.convert xScale myStrategy
            , y <| toString <| Scale.convert yScale value
            , Satt.width <| toString <| Scale.bandwidth xScale
            , Satt.height <| toString <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| toString <| ((Scale.convert (Scale.toRenderable xScale) myStrategy) + 20.0)
            , y <| toString <| Scale.convert yScale value - 5
            , textAnchor "right", stroke "black", fill "black"
            ]
            [ text <| Basics.flip (++) " %" <| toString value ]
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

myMidX: Model -> Float                       
myMidX model =
    let
        allStrat = List.map Tuple.first model.nRange
        allXcoord = List.map (Scale.convert (xScale model.nRange)) allStrat
        myXsum = List.sum allXcoord
        myXlen = List.length allXcoord
               
    in
       ( myXsum / (toFloat myXlen) +  0.5 * (Scale.bandwidth (xScale model.nRange)) + padding)
                                    
                       
bandOffset : Model  -> String
bandOffset model  =
    toString (0.5 * (Scale.bandwidth (xScale model.nRange)) + padding)

            

    
view : Model -> Html Msg
view model =
    let
        regimeBool =
            case model.thisRegime of
                Theory -> True
                Reality -> False
        regimeStyle =
            case model.thisRegime of
                Theory ->
                    [ ("display", "inline")
                    , ("text-decoration", "line-through")
                    ]
                Reality ->
                    [ ("display", "inline") ]
                    
        mySval =
            case model.opacityAnimation of
                Nothing -> model.ciOpacity
                Just a -> A.animate model.currentTick a
        uncalledValue =
            case model.uncalledAnimation of
                Nothing -> Tuple.second ( myFirst model.nValues)
                Just a -> A.animate model.currentTick a
        notifiedValue =
            case model.notifiedAnimation of
                Nothing -> Tuple.second ( mySec model.nValues)
                Just a -> A.animate model.currentTick a
        myValues = [ (Uncalled, uncalledValue)
                   , (Notified, notifiedValue)
                   ]
        myTableWidth =
            case model.tableAnimation of
                Nothing -> model.tableWidth
                Just a -> A.animate model.currentTick a
    in
        Html.div []
            [ Html.div
                  []
                  [ Html.div [ Hatt.class "thePoint"]
                        [ Html.fieldset
                              [ Hatt.class "nullVresearch"
                              , Hatt.style [("padding-left", "35px")]
                              ]
                              [ radio "H_0: \\pi(Fta | \\text{notice}) = \\pi(Fta | \\text{no-notice})" (model.thisRegime == Theory) (SwitchTo Theory)
                              , radio "H_1: \\pi(Fta | \\text{notice}) < \\pi(Fta | \\text{no-notice})" (model.thisRegime == Reality) (SwitchTo Reality)
                              ]
                        ]
                        , Html.div [ Hatt.class "dataDetail"
                                   , Hatt.style [
                                          ("display","inline")
                                         ]
                                   ]
                        [ fieldset [ Hatt.class "ciDetail"
                                   , Hatt.style regimeStyle
                                   , Hatt.disabled regimeBool
                                   ]
                              [ checkbox ToggleCi "Toggle confidence intervals"
                              ]
                        , fieldset [Hatt.class "cmatrixDetail"
                                   , Hatt.style regimeStyle
                                   , Hatt.disabled regimeBool
                                   ]
                            [ checkbox ToggleCMatrix "Toggle confusion matrix"
                            ]
                        , fieldset [ Hatt.class "testDetail"
                                   , Hatt.style regimeStyle
                                   , Hatt.disabled regimeBool
                                   ]
                            [ checkbox ToggleTest  "test"
                        ]
                  ]
                  , Html.div [] []
            , svg [ Satt.width (toString w ++ "px"), Satt.height (toString h ++ "px") ]
                [ Svg.style [(Satt.display "block")]
                      [ text """
                              .column rect { fill: rgba(70, 130, 180, 0.8); }
                              .column text { display: none; }
                              .tick text { font: bold 15px sans-serif;  }
                              .column:hover rect { fill: rgb(70, 130, 180); }
                              .column:hover text { display: inline; }
                              """ ]
                , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                    [ xAxis model.nRange ]
                , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                    [ yAxis,
                          text_
                          [ fontFamily "sans-serif"
                          , fontSize "15"
                          , stroke "black"
                          , x "5"
                          , y "5"
                          ]
                          [
                           text "% Fta"
                          ]
                    ]
                , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), Satt.class "series" ] <|
                    List.map (column (xScale model.nRange)) myValues
                        
                , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), Satt.class "ci" ]
                    [Svg.path [d (makeHline model.nRange (myFirst model.ciUncalled)), stroke "black", strokeOpacity <| toString <| mySval , strokeWidth "2px", fill "none"]
                         []
                    ]
                , text_
                      [ x <| toString <| ((Scale.convert (Scale.toRenderable (xScale model.nRange)) Uncalled) - 75.0)
                      , y <| toString <|Basics.flip (+) 45.0 <| Scale.convert yScale <| Tuple.second <| myFirst model.ciUncalled
                      , textAnchor "right", stroke "white", fill "white", opacity <| toString <| mySval
                      ]
                      [ text <| Basics.flip (++) " %" <| Erd.round 2 (Tuple.second (myFirst model.ciUncalled))]
                          
                , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), Satt.class "ci" ]
                    [Svg.path [d (myCI model.nRange model.ciUncalled), stroke "black", strokeWidth "2px", fill "none", strokeOpacity <| toString <| mySval ]
                         []
                    ]
                      
                , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), Satt.class "ci" ]
                    [Svg.path [d (makeHline model.nRange (mySec model.ciUncalled)), stroke "black", strokeWidth "2px", fill "none", strokeOpacity <| toString <| mySval ]
                         []
                    ]
                , text_
                      [ x <| toString <| ((Scale.convert (Scale.toRenderable (xScale model.nRange)) Uncalled) - 75.0)
                      , y <| toString <|Basics.flip (+) 45.0 <| Scale.convert yScale <| Tuple.second <| mySec model.ciUncalled
                      , textAnchor "right", opacity <| toString <| mySval
                      ]
                      [ text <| Basics.flip (++) " %" <| Erd.round 2 (Tuple.second (mySec model.ciUncalled))]
                          
                , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), Satt.class "ci" ]
                    [Svg.path [d (makeHline model.nRange (myFirst model.ciNotified)), stroke "black", strokeWidth "2px", fill "none", strokeOpacity <| toString <| mySval]
                         []
                    ]
                , text_
                      [ x <| toString <| ((Scale.convert (Scale.toRenderable (xScale model.nRange)) Notified) - 75.0)
                      , y <| toString <|Basics.flip (+) 45.0 <| Scale.convert yScale <| Tuple.second <| myFirst model.ciNotified
                      , textAnchor "right", stroke "white", fill "white", opacity <| toString <| mySval
                      ]
                      [ text <| Basics.flip (++) " %" <| Erd.round 2 (Tuple.second (myFirst model.ciNotified))]
                      
                , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), Satt.class "ci" ]
                    [Svg.path [d (myCI model.nRange model.ciNotified), stroke "black", strokeWidth "2px", fill "none", strokeOpacity <| toString <| mySval ]
                         []
                    ]
                , g [ transform ("translate(" ++ bandOffset model ++ ", " ++ toString padding ++ ")"), Satt.class "ci" ]
                    [Svg.path [d (makeHline model.nRange (mySec model.ciNotified)), stroke "black", strokeWidth "2px", fill "none", strokeOpacity <| toString <| mySval ]
                         []
                    ]
                , text_
                      [ x <| toString <| ((Scale.convert (Scale.toRenderable (xScale model.nRange)) Notified) - 75.0)
                      , y <| toString <|Basics.flip (+) 45.0 <| Scale.convert yScale <| Tuple.second <| mySec model.ciNotified
                      , textAnchor "right", opacity <| toString <| mySval
                      ]
                      [ text <| Basics.flip (++) " %" <| Erd.round 2 (Tuple.second (mySec model.ciNotified))]
                          
                , text_
                      [ translate (myMidX model) (h - padding / 2)
                      , fontFamily "sans-serif"
                      , fontSize "15"
                      , textAnchor "middle"
                      , dy "1em"
                      , stroke "black"
                      ]
                      [ text "Strategy" ]
                ]
            , viewTable model myTableWidth
            , viewTest model
           ]
            ]


viewTable : Model -> Float -> Html Msg
viewTable  model myTableWidth =
    let
        tableStyle =
            case myTableWidth of
                0.0 ->
                    [ ("height","100px")
                    ,  ("border-collapse","collapse")
                    , ("width", "0px")
                    , ("margin-left", "30px")
                    , ("visibility", "hidden")
                    ]
                _ ->
                    [ ("height","100px")
                    ,  ("border-collapse","collapse")
                    , ("width", (++) (toString (Basics.round myTableWidth)) "px")
                    , ("margin-left", "30px")
                    ]

    in
        Html.div [ Hatt.class "p2" ]
            [ Html.table [ Hatt.style tableStyle ]
                  [ Html.thead []
                        [ Html.tr [Hatt.style [ ("border-bottom","1px solid #ddd")
                                              ]
                                  ]
                              [ Html.th [ Hatt.style
                                              [
                                               ("text-align", "left")    
                                              ]
                                        ] [ Html.text "strategy" ]
                              , Html.th [ Hatt.style
                                              [
                                               ("text-align", "right")    
                                              ]
                                        ] [ Html.text "no-warrant-issued" ]
                              , Html.th [ Hatt.style
                                              [
                                               ("text-align", "right")    
                                              ]
                                        ] [ Html.text "warrant-issued" ]
                              ]
                        ]
                  , Html.tr [Hatt.style [ ("border-bottom","1px solid #ddd")
                                        , ("tr:hover", "{background-color: #f5f5f5}")
                                        ]
                            ]
                      [ Html.td [] [ Html.text "no-notification" ]
                      , Html.td
                          [ Hatt.style
                                [
                                 ("text-align", "right")    
                                ]
                          ] [ Html.text <| toString <| selectCases Uncalled NoWarrant model ]
                      , Html.td [ Hatt.style
                                      [
                                       ("text-align", "right")    
                                      ]
                                ] [ Html.text <| toString <| selectCases Uncalled WarrantIss model ]
                      ]
                  , Html.tr [Hatt.style [ ("border-bottom","1px solid #ddd")
                                        , ("tr:hover", "{background-color: #f5f5f5}")            
                                        ]
                            ]
                      [ Html.td [] [ Html.text "notification" ]
                      , Html.td [ Hatt.style
                                      [
                                       ("text-align", "right")    
                                      ]
                                ] [ Html.text <| toString <| selectCases Notified NoWarrant model ]
                      , Html.td [ Hatt.style
                                      [
                                       ("text-align", "right")    
                                      ]
                                ] [ Html.text <| toString <| selectCases Notified WarrantIss model ]
                      ]
                                  
                  ]
            , Html.div [Hatt.style [("height","45px")
                                   , ("width", "40px")
                                   ]
                       ] []
            
            ]
                        
oddsLocale : FormatNumber.Locales.Locale
oddsLocale =
    { usLocale
          | decimals = 0
    }

viewTest : Model -> Html Msg
viewTest  model =
    let
        testStyle =
            case model.showTest of
                True -> 
                    [ ("height","auto")
                    , ("border-collapse","collapse")
                    , ("width", "0px")
                    , ("font-family", "sans-serif")
                    , ("font-size", "20px")
                    , ("display","inline")
                    , ("margin-left", "340px")
                    , ("margin-top", "30px")
                    , ("padding-top", "15px")
                    ]

                False ->
                    [ ("height","100px")
                    , ("border-collapse","collapse")
                    , ("width", "0px")
                    , ("margin-left", "30px")
                    , ("visibility", "hidden")
                    , ("display", "inline")
                    ]

        testBuffStyle = [ ("height","30px")
                        , ("width", "50px")
                        , ("background-color","green") ]
        testString =
            "\\chi^2(1)= " ++ (toString model.chiSq) ++ ", \\text{ }p = 3.7\\text{ x } 10^{-6}"
        oddsString1 =
            "The odds this happened by chance are one in "
        oddsNumFt =
            FormatNumber.format oddsLocale <| 1.0/initialModel.pValue
            
    in
        Html.div [ Hatt.class "sig"
                 , Hatt.style testStyle
                 ]
            [ text <| print <| inline <| testString
            , Html.div [Hatt.class "sigBuff"
                       , Hatt.style [ ("display","block")
                                    , ("height", "30px")
                                    , ("width", "30px")
                                    ]
                       ]
                []
            , Html.div [Hatt.class "sigOdds"
                       , Hatt.style [ ("padding-left","200px") ]
                       ] [text <| oddsString1 ++ oddsNumFt ++ "."]
            ]
            
                        
update: Msg -> Model ->  ( Model, Cmd msg)
update msg model =
        case msg of
            ToggleTest ->
                let
                    myTestStatus = model.showTest
                in
                  
                    ( {model |
                          showTest = not myTestStatus
                      }, Cmd.none)
                                          
            ToggleCi  ->
                  let
                      startOpacity1 = model.ciOpacity
                      endOpacity1 = -1.0 * ( model.ciOpacity - 1.0)
                  in
                      ( { model
                            | ciOpacity = endOpacity1
                            , opacityAnimation = Just (A.animation model.currentTick
                                                     |> A.from startOpacity1
                                                     |> A.to endOpacity1
                                                     |> A.duration (3*second)
                                                     |> ease Ease.inOutElastic
                                                     )
                            , startOpacity = startOpacity1
                            , endOpacity = endOpacity1
                        }, Cmd.none)

            ToggleCMatrix ->
                let
                    startWidth1 = model.tableWidth
                    endWidth1 = -1.0 * (model.tableWidth - 840.0)
                in
                    ( { model
                            | tableWidth = endWidth1
                            , tableAnimation = Just (A.animation model.currentTick
                                                     |> A.from startWidth1
                                                     |> A.to endWidth1
                                                     |> A.duration (3*second)
                                                     |> ease Ease.inOutElastic
                                                     )
                        }, Cmd.none)
                    
            SwitchTo regime ->
                case regime of
                    Theory ->
                        let 
                            startValues = model.nResearch
                            endValues = model.nNull
                            myStart1 = Tuple.second <| myFirst startValues
                            myEnd1   = Tuple.second <| myFirst endValues
                            myStart2 = Tuple.second <| mySec startValues
                            myEnd2   = Tuple.second <| mySec endValues
                        in                
                            ( { model
                                  | nValues = endValues
                                  , uncalledAnimation = Just (A.animation model.currentTick
                                                             |> A.from myStart1
                                                             |> A.to myEnd1
                                                             |> A.duration (3*second)
                                                             |> ease Ease.inOutElastic
                                                             )
                                  , notifiedAnimation = Just (A.animation model.currentTick
                                                             |> A.from myStart2
                                                             |> A.to myEnd2
                                                             |> A.duration (3*second)
                                                             |> ease Ease.inOutElastic
                                                             )
                                  , startUncalled = myStart1
                                  , endUncalled = myEnd1
                                  , startNotified = myStart2
                                  , endUncalled = myEnd2
                                  , thisRegime = Theory
                              }, Cmd.none)
                            
                    Reality ->
                        let 
                            startValues = model.nNull
                            endValues = model.nResearch
                            myStart1 = Tuple.second <| myFirst startValues
                            myEnd1   = Tuple.second <| myFirst endValues
                            myStart2 = Tuple.second <| mySec startValues
                            myEnd2   = Tuple.second <| mySec endValues
                        in
                            ( { model
                                  | nValues = endValues
                                  , uncalledAnimation = Just (A.animation model.currentTick
                                                             |> A.from myStart1
                                                             |> A.to myEnd1
                                                             |> A.duration (3*second)
                                                             |> ease Ease.inOutElastic
                                                             )
                                  , notifiedAnimation = Just (A.animation model.currentTick
                                                             |> A.from myStart2
                                                             |> A.to myEnd2
                                                             |> A.duration (3*second)
                                                             |> ease Ease.inOutElastic
                                                             )
                                  , startUncalled = myStart1
                                  , endUncalled = myEnd1
                                  , startNotified = myStart2
                                  , endUncalled = myEnd2
                                  , thisRegime = Reality
                              }, Cmd.none)

            CurrentTick time ->
                ( { model | currentTick = time }, Cmd.none)
        
radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    label
        [ Hatt.style [("padding", "20px")]
    ]
    [ Html.input [ Hatt.type_ "radio", Hatt.name "nullVreality", Hevent.onClick msg, checked isChecked ] []
    , text <| print <| inline value
    ]

checkbox : msg -> String -> Html msg
checkbox msg name =
  label
    [ Hatt.style [("padding", "20px")]
    ]
    [ Html.input [ Hatt.type_ "checkbox", onClick msg ] []
    , text name
    ]
                    
main : Program Never Model Msg
main =
    Html.program
        { init = (initialModel, Cmd.none)
        , update = update
        , view = view
        , subscriptions = (\_ -> Af.times CurrentTick)
        }
