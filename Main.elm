module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array
import Tuple


main =
    Html.beginnerProgram
        { model = initModel
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
    | Winner Player


type Player
    = User
    | CPU


type alias Box =
    { id : Int
    , player : Maybe Player
    }


initModel : Model
initModel =
    Model NewGame
        User
        [ [ Box 1 Nothing, Box 2 Nothing, Box 3 Nothing ]
        , [ Box 4 Nothing, Box 5 Nothing, Box 6 Nothing ]
        , [ Box 7 Nothing, Box 8 Nothing, Box 9 Nothing ]
        ]


cpuFirstInitModel : Model
cpuFirstInitModel =
    Model NewGame
        User
        [ [ Box 1 Nothing, Box 2 Nothing, Box 3 Nothing ]
        , [ Box 4 Nothing, Box 5 (Just CPU), Box 6 Nothing ]
        , [ Box 7 Nothing, Box 8 Nothing, Box 9 Nothing ]
        ]



-- UPDATE


type Msg
    = Click Int
    | RestartWithCpuFirst
    | RestartWithUserFirst


update : Msg -> Model -> Model
update msg model =
    let
        newModel =
            case msg of
                Click id ->
                    if model.turn == User then
                        boxClicked model id
                    else
                        model

                RestartWithCpuFirst ->
                    cpuFirstInitModel

                RestartWithUserFirst ->
                    initModel
    in
        newModel |> updateAppState |> cpuTurn |> updateAppState


boxClicked : Model -> Int -> Model
boxClicked model id =
    let
        clickedBox =
            List.head <| List.filter (\box -> box.id == id) (List.concat model.board)

        gameIsNotOver =
            model.appState == NewGame
    in
        case clickedBox of
            Just box ->
                if box.player == Nothing && gameIsNotOver then
                    { model
                        | turn = rotateTurn model.turn
                        , board = flipBoxById model.board id model.turn
                    }
                else
                    model

            Nothing ->
                -- need error handling here???
                model


flipBoxById : List (List Box) -> Int -> Player -> List (List Box)
flipBoxById currentBoard boxId player =
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
        User ->
            CPU

        CPU ->
            User


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
            if List.member (Just (Winner User)) gameResults then
                Winner User
            else if List.member (Just (Winner CPU)) gameResults then
                Winner CPU
            else if List.member (Just Draw) gameResults then
                Draw
            else
                NewGame
    in
        { model | appState = newAppState }


boxValue : Box -> Int
boxValue box =
    case box.player of
        Just User ->
            1

        Just CPU ->
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
            Just (Winner User)
        else if List.member -3 diagonalCountList then
            Just (Winner CPU)
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
            Just (Winner User)
        else if List.member -3 rowCountList then
            Just (Winner CPU)
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
                    (List.sum << (List.map (Maybe.map boxValue >> Maybe.withDefault 0)))
    in
        if List.member 3 columnCountList then
            Just (Winner User)
        else if List.member -3 columnCountList then
            Just (Winner CPU)
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


cpuTurn : Model -> Model
cpuTurn model =
    if model.appState == NewGame && model.turn == CPU then
        boxClicked model (minimax model)
    else
        model


minimax : Model -> Int
minimax model =
    let
        possibleMoves : List Int
        possibleMoves =
            model.board
                |> List.concat
                |> List.filter (\box -> box.player == Nothing)
                |> List.map .id

        getModelFromNextMove : Int -> Model
        getModelFromNextMove boxId =
            boxClicked model boxId |> updateAppState

        resolvedMoves : List ( Int, AppState )
        resolvedMoves =
            possibleMoves
                |> List.map (\idx -> ( idx, resolveTree (getModelFromNextMove idx) ))

        winningMoves : List ( Int, AppState )
        winningMoves =
            resolvedMoves
                |> List.filter (\( _, appState ) -> appState == Winner model.turn)

        drawMoves : List ( Int, AppState )
        drawMoves =
            resolvedMoves
                |> List.filter (\( _, appState ) -> appState == Draw)

        _ =
            Debug.log "resolvedMoves" resolvedMoves
    in
        if List.length winningMoves /= 0 then
            List.head winningMoves |> Maybe.withDefault (( 1, NewGame )) |> Tuple.first
        else if List.length drawMoves /= 0 then
            List.head drawMoves |> Maybe.withDefault (( 1, NewGame )) |> Tuple.first
        else
            Debug.crash "unexpected state" -1


resolveTree : Model -> AppState
resolveTree model =
    let
        currentPlayer : Player
        currentPlayer =
            model.turn

        otherPlayer : Player
        otherPlayer =
            if currentPlayer == User then
                CPU
            else
                User

        possibleMoves : List Int
        possibleMoves =
            model.board
                |> List.concat
                |> List.filter (\box -> box.player == Nothing)
                |> List.map .id

        possibleModels : List Model
        possibleModels =
            possibleMoves
                |> List.map (boxClicked model)
                |> List.map updateAppState

        branchResults : List AppState
        branchResults =
            possibleModels
                |> List.map
                    (\model ->
                        if model.appState == NewGame then
                            resolveTree model
                        else
                            model.appState
                    )
    in
        if List.member (Winner currentPlayer) branchResults then
            Winner currentPlayer
        else if List.member Draw branchResults then
            Draw
        else
            Winner otherPlayer



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ gameStateDiv model
        , div [ class "board" ] (renderBoard model)
        , renderGameControls
        ]


gameStateDiv : Model -> Html Msg
gameStateDiv model =
    case model.appState of
        Winner p ->
            div [] [ text <| (playerToText (Just p)) ++ " wins!" ]

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


renderGameControls : Html Msg
renderGameControls =
    div [ class "controls" ]
        [ button [ class "restart--cpu-first", onClick RestartWithCpuFirst ] [ text "Restart AI first" ]
        , button [ class "restart--user-first", onClick RestartWithUserFirst ] [ text "Restart me first" ]
        ]


playerToText : Maybe Player -> String
playerToText player =
    case player of
        Just User ->
            "X"

        Just CPU ->
            "O"

        Nothing ->
            ""
