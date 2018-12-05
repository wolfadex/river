module Main exposing (main)


import Browser exposing (Document)
import Color exposing (Color)
import Debug exposing (log)
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
-- import Html exposing (Html)
import Html.Attributes as Attrs
-- import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Material.Icons.Action as IconAction
import Set exposing (Set)
import Svg exposing (Svg)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


---- TYPES ----


type alias Model =
    { project : HistoryProject
    , projectView : ProjectView
    , moduleView : ModuleView
    }


type alias HistoryProject = (List Project, Project, List Project)


type Msg
    = NoOp
    | Undo
    | Redo
    | ChangeProjectName String
    | SetProjectView ProjectView
    | SetProjectType ProjectType


type alias Project =
    { name : String
    , type_ : ProjectType
    }


type ProjectType
    = App Application
    | Lib


type alias Application =
    { modules : Set Module
    }


type alias Module =
    { name : String
    , functions : Set Function
    }


type alias Function =
    { name : String }


type ProjectView
    = VApp
    | VModules
    | VSettings


type ModuleView
    = VFunctions
    | VTypes
    | VAliases


---- INIT ----


init : () -> (Model, Cmd Msg)
init flags =
    ( { project = ([], { name = "New Project", type_ = Lib}, [])
      , projectView = VApp
      , moduleView = VFunctions
      }
    , Cmd.none
    -- , Cmd.batch
    --       []
    )


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
    -- Sub.batch
    --     [ onKeyPress decodeKeyPress
    --     ]


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ project } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Undo ->
            ( { model | project = undo project }, Cmd.none )
        Redo ->
            ( { model | project = redo project }, Cmd.none )
        SetProjectView v ->
            ( { model | projectView = v }
            , Cmd.none
            )
        ChangeProjectName newName ->
            updatePoject (\p -> { p | name = newName }) model
        SetProjectType pt ->
            updatePoject (\p -> { p | type_ = pt }) model


updatePoject : (Project -> Project) -> Model -> ( Model, Cmd Msg )
updatePoject updater ({ project } as model) =
    let
        ( past, present, future ) = project
        newProject =
            ( present :: List.take 10 past
            , updater present
            , []
            )
    in
        ( { model | project = newProject }
        , Cmd.none
        )


undo : HistoryProject -> HistoryProject
undo ( past, present, future ) =
    let
        nextPresent = List.head past
    in
        ( List.drop 1 past
        , case nextPresent of
              Nothing -> present
              Just p -> p
        , case nextPresent of
              Nothing -> future
              Just _ -> present :: future
        )


redo : HistoryProject -> HistoryProject
redo ( past, present, future ) =
    let
        nextPresent = List.head future
    in
        ( case nextPresent of
              Nothing -> past
              Just _ -> present :: past
        , case nextPresent of
              Nothing -> present
              Just p -> p
        , List.drop 1 future
        )


---- VIEW ----


view : Model -> Document Msg
view { project, projectView } =
    let
        (past, present, future) = project
        hasPast = List.length past > 0
        hasFuture = List.length future > 0
    in
        { title = "River"
        , body =
              [ El.layout
                    [ El.width El.fill
                    , El.height El.fill
                    ]
                    <| El.row
                        [ El.height El.fill
                        , El.width El.fill
                        ]
                        [ viewTabs  projectView
                        , El.column
                              [ El.width El.fill
                              , El.height El.fill
                              ]
                              [ header hasPast hasFuture
                              , editorBody
                                    <| case projectView of
                                        VApp ->
                                            appView present
                                        VModules ->
                                            El.none
                                        VSettings ->
                                            El.none
                              ]
                        ]
              ]
        }


scaled : Int -> Int
scaled n =
    floor <| El.modular 16 1.25 n


viewTabs : ProjectView -> Element Msg
viewTabs projectView =
    El.column
        [ El.height El.fill
        , El.width El.shrink
        , El.paddingXY (scaled -2) (scaled 2)
        , Background.color <| El.rgb255 190 40 60
        , El.spacing <| scaled 1
        ]
        [ tab (projectView == VApp) IconAction.assignment VApp
        , tab (projectView == VModules) IconAction.group_work VModules
        , tab (projectView == VSettings) IconAction.settings VSettings
        ]


tab : Bool -> (Color -> Int -> Svg Msg) -> ProjectView -> Element Msg
tab active label nextView =
    let
        color = if active then white else black  
    in
        circleButton (icon label color (scaled 3)) color (Just <| SetProjectView nextView)


black : (Int, Int, Int)
black =
    (0, 0, 0)


white : (Int, Int, Int)
white =
    (255, 255, 255)


header : Bool -> Bool -> Element Msg
header canUndo canRedo =
    El.row
        [ Background.color <| El.rgb255 60 200 90
        , El.paddingXY (scaled 2) (scaled -2)
        , El.width El.fill
        , El.spacingXY (scaled 1) 0
        ]
        [ Input.button
              (El.alignRight :: buttonStyle)
              { onPress =
                    if canUndo then
                        Just Undo
                    else
                        Nothing
              , label = El.text "Undo"
              }
        , Input.button
              buttonStyle
              { onPress =
                    if canRedo then
                        Just Redo
                    else
                        Nothing
              , label = El.text "Redo"
              }
        ]


buttonStyle : List (El.Attribute msg)
buttonStyle =
    [ El.paddingXY 16 8
    , Border.width 1
    , Border.color <| El.rgb255 0 0 0
    ]


icon : (Color -> Int -> Svg Msg) -> (Int, Int, Int) -> Int -> Element Msg
icon i (r, g, b) size =
    El.html
        <| Svg.svg
            [ Attrs.style "width" (String.fromInt size ++ "px")
            , Attrs.style "height" (String.fromInt size ++ "px")
            ]
            [i (Color.rgb255 r g b) size]


circleButton : Element Msg -> (Int, Int, Int) -> Maybe Msg -> Element Msg
circleButton label (r, g, b) pressHandler =
    Input.button
        [ Border.width 1
        , Border.color <| El.rgb255 r g b
        , Border.rounded 100
        , El.width El.shrink
        , El.height El.shrink
        , El.padding <| scaled -3
        ]
        { onPress = pressHandler
        , label = label
        }


editorBody : Element Msg -> Element Msg
editorBody child =
    El.el
        [ El.padding <| scaled -2 ]
        child


appView : Project -> Element Msg
appView { name, type_ } =
    El.column
        []
        [ Input.text
              [ El.width <| El.minimum (scaled 16) El.fill
              ]
              { onChange = ChangeProjectName
              , text = name
              , placeholder = Nothing
              , label = Input.labelAbove [] <| El.text "Project Name"
              }
        , Input.radioRow
              []
              { onChange = SetProjectType
              , selected = Just type_
              , label = Input.labelAbove [] <| El.text "Project Type"
              , options =
                    [ Input.option App (El.text "Application")
                    -- , Input.option Lib (El.text "Library")
                    ]
              }
        ]
    