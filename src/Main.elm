module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, br,h1)
import Html.Events as HEV exposing (onClick, custom)
import Html.Attributes exposing (style)
import Html.Attributes as A
import Html.Events as E
import Random

import Mine
import Random exposing (Generator)

type alias Cell =
    { id : Int, isMine : Bool, revealed : Bool, flagged: Bool }

type alias Model = {cells: List Cell, died: Bool, won: Bool}

type alias CellID = Int

emptyCell : { id : number, isMine : Bool, revealed : Bool, flagged : Bool }
emptyCell = {id=0, isMine=False, revealed=False, flagged=False}

init: () -> (Model, Cmd Msg)
init _ = ({
       cells = [],      
       died=False,
      won=False
    },exampleGenerateRandomMines)

exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 100
        , height = 100
        , minMines = 10
        , maxMines = 30
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

--Mettre une bombe aux coordonnées de la liste MinesGenerated
putBomb : Int -> List(Int) ->Bool
putBomb x l =
    if List.member x l then
        True
    else
        False

type Msg
    = MinesGenerated (List ( Int)) 
    | NewGame
    | Reveal CellID 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MinesGenerated listbomb -> ({ model | cells = List.map (\x -> { id = x, isMine = putBomb x listbomb, revealed = False, flagged = False })(List.range 1 100)}, Cmd.none)
        Reveal id ->
            ({ model
                | cells = model.cells |> List.map (\cell -> if id == cell.id then
                                                                { cell | revealed = True }
                                                            else
                                                                cell )},Cmd.none)        
        NewGame -> init()
        --Reveal n -> (checkLoose (revealAll model n), Cmd.none)

checkLoose: Model -> Model
checkLoose model = {model | died = (List.foldl (\cell died -> died && ((cell.isMine && cell.revealed))) True model.cells)}

-- Cell revealing
revealAll: Model -> CellID -> Model
revealAll model id = 
  if (getCell model id).id==0 && not (getCell model id).isMine then
    {model | cells = 
      (List.indexedMap
        (revealMap (reveal model id []))
        model.cells
      )
    }
  else 
    {model | cells = 
      (List.indexedMap
        (revealMapOne id)
        model.cells
      ),
      died = model.died || (getCell model id).isMine
    }
    
---------------------------------------------------------------------------------------------     
revealMapOne: CellID -> CellID -> Cell -> Cell
revealMapOne revealedId id cell =
  if revealedId == id then
    {cell | revealed=True}
  else 
    cell
  
revealMap: List CellID -> CellID -> Cell -> Cell
revealMap revealed id cell =
  if (List.member id revealed) then
    {cell | revealed=True}
  else 
    cell
    
reveal: Model -> CellID -> List CellID -> List CellID 
reveal model id revealed =
  List.foldl
    (revealFilter model id)
    revealed
    (List.indexedMap Tuple.pair model.cells)
    
revealFilter: Model -> CellID -> (CellID, Cell) -> List CellID -> List CellID 
revealFilter model revealedId idAndCell revealed =
  if (
    not (List.member (Tuple.first idAndCell) revealed) &&
    abs ((getX (Tuple.first idAndCell)) - (getX revealedId)) <= 1 && 
    abs ((getY (Tuple.first idAndCell)) - (getY revealedId)) <= 1 
  ) then (
    if (Tuple.second idAndCell).id==0 then
      reveal model (Tuple.first idAndCell) ((Tuple.first idAndCell) :: revealed)
    else
      (Tuple.first idAndCell) :: revealed
  ) else
    revealed

------------------------------------------------------------------

view : Model -> Html Msg
view model =
        if model.died == False then
            div[]
            [ h1 [] [ text "Minesweeper" ]
            , text "Play !",
            div [ A.class "myGrid"]
            (List.map (viewCell model.cells)model.cells), br[][],button [A.class "button", onClick NewGame] [text "Restart"] ]
        else
            h1 [] [ text "Minesweeper" ]
            

main : Program () Model Msg
main =
  Browser.element { init = init, update = update, subscriptions = always Sub.none, view = view }


numberOfBombsAround : List Cell -> Int -> Int
numberOfBombsAround cells id =
    let
        leftNeighbours leftCells x =
            List.filter (\cell -> abs ((cell.id - 10) - x) == 1 && cell.isMine) leftCells

        rightNeighbours rightCells x =
            List.filter (\cell -> abs ((cell.id + 10) - x) == 1 && cell.isMine) rightCells

        neighbours asideCells x =
            List.filter (\cell -> abs (cell.id - x) == 1 && cell.isMine) asideCells

        upDownNeighbours upDownCells x =
            List.filter (\cell -> abs (cell.id - x) == 10 && cell.isMine) upDownCells

        inTheGrid inCells =
            List.filter (\cell -> cell.id > 0 && cell.id < 101) inCells
    in
    List.append (leftNeighbours (inTheGrid cells) id) (rightNeighbours (inTheGrid cells) id)
        |> List.append (neighbours (inTheGrid cells) id)
        |> List.append (upDownNeighbours (inTheGrid cells) id)
        |> List.length


viewCell : List Cell -> Cell -> Html Msg
viewCell cells cell =
    button
        [  onClick (Reveal cell.id), A.style "width" "50px", A.style "height" "50px" ]
        [ if cell.revealed then
            if cell.isMine then
                text "💣"

            else
                text (String.fromInt (numberOfBombsAround cells cell.id))

          else
            text ""
        ]

revealIfId : Int -> Cell -> Cell
revealIfId id cell =
    if cell.id /= id then
        cell
    else
        { id = cell.id, isMine = cell.isMine, revealed = True, flagged=False }

isRevealed : Cell -> Cell
isRevealed cell =
    cell

-- utils
getX: CellID -> Int
getX index = modBy 10 index

getY: CellID -> Int
getY index = index // 10

getCellXY: Model -> Int -> Int -> Cell
getCellXY model x y = getCell model (y * 10 + x)

getCell: Model -> CellID -> Cell
getCell model id = 
  Maybe.withDefault
    emptyCell
    (List.head (List.drop id model.cells))

newLine: CellID -> Html Msg -> List (Html Msg)
newLine index el= 
  if getX (index + 1) == 0 then
    [el, br [] []]
  else
    [el]
