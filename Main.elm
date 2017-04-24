module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { turn : Player
    , board : List (List Box)
    }


type Player
    = X
    | O


type alias Box =
    { id : Int
    , player : Maybe Player
    }


model : Model
model =
    Model X
        [ [ Box 1 Nothing, Box 2 Nothing, Box 3 Nothing ]
        , [ Box 4 Nothing, Box 5 Nothing, Box 6 Nothing ]
        , [ Box 7 Nothing, Box 8 Nothing, Box 9 Nothing ]
        ]



-- UPDATE


type Msg
    = Click Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click id ->
            let
                clickedBox =
                    List.head <| List.filter (\box -> box.id == id) (List.concat model.board)
            in
                case clickedBox of
                    Just box ->
                        if box.player == Nothing then
                            { model | board = updateBoard model.board id model.turn }
                        else
                            model

                    Nothing ->
                        -- need error handling here???
                        model


updateBoard : List (List Box) -> Int -> Player -> List (List Box)
updateBoard currentBoard boxId player =
    currentBoard
        |> List.map
            (\boxList ->
                boxList
                    |> List.map
                        (\box ->
                            if box.id == boxId then
                                Box box.id (Just player)
                            else
                                box
                        )
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [] (renderBoard model)


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
