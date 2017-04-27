module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { appState : AppState
    , turn : Player
    , board : List (List Box)
    }


type AppState
    = Draw
    | NewGame
    | XWin
    | OWin


type Player
    = X
    | O


type alias Box =
    { id : Int
    , player : Maybe Player
    }


model : Model
model =
    Model NewGame
        X
        [ [ Box 1 Nothing, Box 2 Nothing, Box 3 Nothing ]
        , [ Box 4 Nothing, Box 5 Nothing, Box 6 Nothing ]
        , [ Box 7 Nothing, Box 8 Nothing, Box 9 Nothing ]
        ]



-- UPDATE


type Msg
    = Click Int


update : Msg -> Model -> Model
update msg model =
    let
        newModel =
            case msg of
                Click id ->
                    let
                        clickedBox =
                            List.head <| List.filter (\box -> box.id == id) (List.concat model.board)
                    in
                        case clickedBox of
                            Just box ->
                                if box.player == Nothing then
                                    { model
                                        | turn = rotateTurn model.turn
                                        , board = updateBoard model.board id model.turn
                                    }
                                else
                                    model

                            Nothing ->
                                -- need error handling here???
                                model
    in
        newModel |> updateAppState


updateBoard : List (List Box) -> Int -> Player -> List (List Box)
updateBoard currentBoard boxId player =
    let
        updateBox box =
            if box.id == boxId then
                Box box.id (Just player)
            else
                box
    in
        currentBoard
            |> List.map (List.map updateBox)


rotateTurn : Player -> Player
rotateTurn currentPlayer =
    case currentPlayer of
        X ->
            O

        O ->
            X


updateAppState : Model -> Model
updateAppState model =
    let
        board =
            model.board

        gameWinChecks =
            [ checkRowWin, checkColumnWin, checkDiagonalWin, checkDraw ]

        gameResults =
            List.map ((|>) board) gameWinChecks

        newAppState =
            if List.member (Just XWin) gameResults then
                XWin
            else if List.member (Just OWin) gameResults then
                OWin
            else if List.member (Just Draw) gameResults then
                Draw
            else
                NewGame
    in
        { model | appState = newAppState }


boxValue : Box -> Int
boxValue box =
    case box.player of
        Just X ->
            1

        Just O ->
            -1

        Nothing ->
            0


checkDiagonalWin : List (List Box) -> Maybe AppState
checkDiagonalWin board =
    let
        getElementAt : ( Int, Int ) -> Maybe Box
        getElementAt ( row, col ) =
            board
                |> List.concat
                |> List.filter (\box -> box.id == row * 3 + (col + 1))
                |> List.head

        diagonal1 =
            [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]

        diagonal2 =
            [ ( 0, 2 ), ( 1, 1 ), ( 2, 0 ) ]

        diagonal1Boxes : List (Maybe Box)
        diagonal1Boxes =
            diagonal1
                |> List.map ((<|) getElementAt)

        diagonal2Boxes : List (Maybe Box)
        diagonal2Boxes =
            diagonal2
                |> List.map ((<|) getElementAt)

        diagonalCountList : List Int
        diagonalCountList =
            [ diagonal1Boxes, diagonal2Boxes ]
                |> List.map
                    (List.map (Maybe.map boxValue))
                |> List.map
                    (List.map (Maybe.withDefault 0))
                |> List.map List.sum
    in
        if List.member 3 diagonalCountList then
            Just XWin
        else if List.member -3 diagonalCountList then
            Just OWin
        else
            Nothing


checkRowWin : List (List Box) -> Maybe AppState
checkRowWin board =
    let
        rowCountList : List Int
        rowCountList =
            board
                |> List.map
                    (List.map boxValue)
                |> List.map List.sum
    in
        if List.member 3 rowCountList then
            Just XWin
        else if List.member -3 rowCountList then
            Just OWin
        else
            Nothing


checkColumnWin : List (List Box) -> Maybe AppState
checkColumnWin board =
    let
        getElementAt : List a -> Int -> Maybe a
        getElementAt list idx =
            List.head (List.drop idx list)

        getColumn : Int -> List (Maybe Box)
        getColumn colIdx =
            List.map ((flip getElementAt) colIdx) board

        columnCountList : List Int
        columnCountList =
            List.map (getColumn) [ 0, 1, 2 ]
                |> List.map
                    (List.map (Maybe.map boxValue))
                |> List.map
                    (List.map (Maybe.withDefault 0))
                |> List.map List.sum
    in
        if List.member 3 columnCountList then
            Just XWin
        else if List.member -3 columnCountList then
            Just OWin
        else
            Nothing


checkDraw : List (List Box) -> Maybe AppState
checkDraw board =
    let
        usedBoxCount : Int
        usedBoxCount =
            board
                |> List.concat
                |> List.filter (\box -> box.player /= Nothing)
                |> List.length
    in
        case usedBoxCount of
            9 ->
                Just Draw

            _ ->
                Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ gameStateDiv model
        , div [ class "board" ] (renderBoard model)
        ]


gameStateDiv : Model -> Html Msg
gameStateDiv model =
    case model.appState of
        XWin ->
            div [] [ text "X win!" ]

        OWin ->
            div [] [ text "O win!" ]

        Draw ->
            div [] [ text "Draw!" ]

        _ ->
            text ""


renderBoard : Model -> List (Html Msg)
renderBoard model =
    List.map boardRow model.board


boardRow : List Box -> Html Msg
boardRow boxList =
    div [ class "row" ] (List.map boardBox boxList)


boardBox : Box -> Html Msg
boardBox box =
    button [ class "box", onClick (Click box.id) ] [ text (playerToText box.player) ]


playerToText : Maybe Player -> String
playerToText player =
    case player of
        Just X ->
            "X"

        Just O ->
            "O"

        Nothing ->
            ""
