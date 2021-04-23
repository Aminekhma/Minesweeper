module Main exposing (..)
-- Minesweeper game for elm, try it out at: https://elm-lang.org/examples/groceries (paste this code there)

import Browser
import Html exposing (Html, button, div, text, br,h1)
import Html.Events as HEV exposing (onClick, custom)
import Html.Attributes exposing (style)
import Html.Attributes as A
import Html.Events as E

import Json.Decode as Json
import Random
import Mine
import Random exposing (Generator)


width = 10
height = 10
totalBombs = 10
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = boardView }
 
type alias Cell = {value: Int, isBomb: Bool, revealed: Bool, flagged: Bool} 
type alias CellID = Int
type alias Model = {available: List Int, cells: List Cell, died: Bool, won: Bool}

type Msg = Reset | Generate Int | Reveal CellID | Flag CellID

emptyCell = {value=0, isBomb=False, revealed=False, flagged=False}

-- Updating state
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> init ()
    Generate id -> (
      addBomb model (get id model.available),
      getBombIndex (List.length model.available - 1))
    Reveal id -> (checkWin (revealAll model id), Cmd.none)
    Flag id -> (
      {model | cells=
        (List.indexedMap
          (\index cell -> 
            if index == id then
              {cell | flagged = not cell.flagged}
            else
              cell
          )
          model.cells
        )
      }, 
      Cmd.none)

checkWin: Model -> Model
checkWin model = 
  {model | won=
    (List.foldl
      (\cell won -> 
        won && ((cell.isBomb && not cell.revealed) || cell.revealed)
      )
      True
      model.cells
    )
  }
    
-- Cell revealing
revealAll: Model -> CellID -> Model
revealAll model id = 
  if (getCell model id).value==0 && not (getCell model id).isBomb then
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
      died = model.died || (getCell model id).isBomb
    }
    
     
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
    if (Tuple.second idAndCell).value==0 then
      reveal model (Tuple.first idAndCell) ((Tuple.first idAndCell) :: revealed)
    else
      (Tuple.first idAndCell) :: revealed
  ) else
    revealed
    
-- Field init
init: () -> (Model, Cmd Msg)
init _ = (
    {
      available =
        (generateNumbers 
          (width * height - 1)
        ),
      cells = 
        (List.repeat 
          (width * height) 
          emptyCell
        ),
      died=False,
      won=False
    },
    getBombIndex (width * height)
  )
        
getBombIndex: Int -> Cmd Msg
getBombIndex availableCount = 
  if (width * height) - availableCount < totalBombs then
    Random.generate
      Generate
      (Random.int 
        0 
        (availableCount - 1)
      )
 else
   Cmd.none
    
        
addBomb: Model -> CellID -> Model
addBomb model id = {
    model |
    available = 
      (List.filter 
        (notEquals id) 
        model.available
      ),
    cells = 
      (List.indexedMap
        (addBombAt 
          model.cells 
          (getX id) 
          (getY id)
        )
        model.cells
      )
  }
  
addBombAt: List Cell -> Int -> Int -> Int -> Cell -> Cell
addBombAt cells x y cellIndex cell = 
  if (getX cellIndex) == x && (getY cellIndex) == y then
    {cell | isBomb = True}
  else if abs ((getX cellIndex) - x) <= 1 && abs ((getY cellIndex) - y) <= 1 then
    {cell | value = cell.value + 1}
  else
    cell
  
-- utils
getX: CellID -> Int
getX index = modBy width index

getY: CellID -> Int
getY index = index // width

getCellXY: Model -> Int -> Int -> Cell
getCellXY model x y = getCell model (y * width + x)

getCell: Model -> CellID -> Cell
getCell model id = 
  Maybe.withDefault
    emptyCell
    (List.head
      (List.drop
        id
        model.cells
      )
    )

notEquals: a -> a -> Bool
notEquals a b = a /= b

generateNumbers: Int -> List Int
generateNumbers remaining = 
  if remaining == -1 then
    []
  else
    remaining :: (generateNumbers (remaining - 1))
    
get: Int -> List number -> number
get index list = 
  Maybe.withDefault
    0
    (List.head
      (List.drop
        index
        list
      )
    )
    

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    HEV.custom "contextmenu"
        (Json.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
        
boardView: Model -> Html Msg
boardView model = 
  div 
    [
    style "background" "rgb(0, 70, 128)",
    style "background-position" "center",
    style "background-size" "cover",
    style "min-height" "100vh",
    style "overflow" "hidden",
    style "background-blend-mode" "overlay"]
    (          

      (if model.died then
        [
          div [] [h1 [
            style "text-align" "center", 
            style "background-color" "red",
            style "font-weight" "600",
            style "color" "black",
            style "padding" "10px 10px",
            style "top" "60px",
            style "left" "35%"             
             ] [ text "Game Over"]],  
             br [] [],
          button [onClick Reset,
            style "position" "absolute",
            style "color" "#fff",
            style "font-weight" "600",
            style "text-align" "center",
            style "width" "150px",
            style "height" "40px",
            style "background" "black",
            style "z-index" "1",
            style "cursor" "pointer",
            style "border-bottom-right-radius" "3px",
            style "border-top-right-radius" "3px",
            style "border-radius" "50px",
            style "margin" "0",
            style "left" "45%",
            style "position" "absolute",
            style "top" "12%" 
              ] [text "RESTART"],
          br [] []
        ]
      else if model.won then
        [
          div [] [h1 [
            style "text-align" "center", 
            style "background-color" "green",
            style "font-weight" "600",
            style "color" "white",
            style "padding" "10px 10px",
            style "top" "60px",
            style "left" "35%"             
             ] [ text "YOU WON !"]],br [][],
          button [onClick Reset,
            style "position" "absolute",
            style "color" "#fff",
            style "font-weight" "600",
            style "text-align" "center",
            style "width" "150px",
            style "height" "40px",
            style "background" "black",
            style "z-index" "1",
            style "cursor" "pointer",
            style "border-bottom-right-radius" "3px",
            style "border-top-right-radius" "3px",
            style "border-radius" "50px",
            style "margin" "0",
            style "left" "45%",
            style "position" "absolute",
            style "top" "12%" 
              ] [text "RESTART"],
          br [] []
        ]
      else
        [h1 [
            style "text-align" "center", 
            style "background-color" "black",
            style "font-weight" "600",
            style "color" "#fff",
            style "padding" "10px 10px",
            style "top" "60px",
            style "left" "35%"             
             ] [ text "Minesweeper"]]
      ) ++
      (List.concat
        (List.indexedMap 
          (cellView model) 
          model.cells
        )
      )
    )
      

cellStyle = 
  [
    style "vertical-align" "top",
    style "grid-template-columns" "repeat(10, 50px)",
    style "grid-auto-rows" "auto",
    style "z-index" "1",
    style "zcursor" "pointer",
    style "zright" "0",
    style "zborder-bottom-right-radius" "3px",
    style "zborder-top-right-radius" "3px",
    style "zborder-radius" "50px",
    style "zdisplay" "flex",
    style "text-align" "center",
    style "vertical-align" "middle",
    style "margin" "0 auto",
    style "top" "20px",
    style "left" "34%",
    style "position" "relative",
    style "width" "50px",
    style "height" "50px",
    style "margin" "0 auto",
    style "padding" "0",
    style "display" "inline-block",
    style "line-height" "50px",
    style "text-align" "center",
    style "color" "black",
    style "font-weight" "600",
    style " border" "1px dotted black"
  ]

cellView: Model -> CellID -> Cell -> List (Html Msg)
cellView model id cell = 
  newLine 
    id
    (
      if cell.revealed then (
        if cell.isBomb then 
          div ([style "background-color" "red"] ++ cellStyle) [text "💣"]
        else
          button ([
            style "background-color" "green"
          ] ++ cellStyle) [
            text (String.fromInt cell.value)
          ]
      ) else if cell.flagged then
        button ([
          style "background-color" "orange",
          onRightClick (Flag id)
        ] ++ cellStyle) [text "F"]
      else 
        button ([ 
          style "background-color" "rgb(172, 171, 171)"
        ] ++ cellStyle ++ (
          if not model.died then 
            [
              onClick (Reveal id),
              onRightClick (Flag id)
            ]
          else
            []
        )) []
    )
    
newLine: CellID -> Html Msg -> List (Html Msg)
newLine index el= 
  if getX (index + 1) == 0 then
    [el, br [] []]
  else
    [el]