import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color
import Time exposing (..)
import Matrix exposing (Matrix)
import Array exposing (Array)
import Random exposing (..)
import Random.Bool exposing (..)
import Signal.Extra as SignalE
import Debug

-- Model
type alias Cell = Bool
type alias Model = Matrix Cell

initialModel : Time -> Model
initialModel time =
  let seed = initialSeed (round time)
      rowGenerator = list 30 bool
      worldGenerator = list 30 rowGenerator
      (world, _) = generate worldGenerator seed
  in
     Matrix.fromList (List.map (List.map not) world)



-- Update

update : Time -> Model -> Model
update _ model =
  Matrix.indexedMap (\x y cell -> nextGen (x, y) cell model) model

nextGen : (Int, Int) -> Cell -> Model -> Cell
nextGen (x, y) cell world =
  let cellMToCell cellM =
        case cellM of
          Just c -> c
          Nothing -> Debug.crash "out of bounds"
      neighbors = neighborsM (x,y) world |> List.map cellMToCell
      liveCells =  List.filter identity neighbors
      liveCellCount = List.length liveCells
  in
     if | cell && liveCellCount < 2 -> False
        | cell && (liveCellCount == 2 || liveCellCount == 3) -> True
        | cell && liveCellCount > 3 -> False
        | not cell && liveCellCount == 3 -> True
        | otherwise -> False

neighborsM : (Int, Int) -> Model -> List (Maybe Cell)
neighborsM (x, y) world =
  let width = Matrix.width world
      height = Matrix.height world
  in
     [ Matrix.get (x - 1) (y - 1) world -- NW
     , Matrix.get x (y - 1) world -- N
     , Matrix.get ((x + 1) % width) (y - 1) world -- NE
     , Matrix.get (x - 1) y world -- W
     , Matrix.get ((x + 1) % width) y world -- E
     , Matrix.get (x - 1) ((y + 1) % height) world -- SW
     , Matrix.get x ((y + 1) % height) world -- S
     , Matrix.get ((x + 1) % width) ((y + 1) % height) world -- SE
     ]

-- View

view : Model -> Element
view model =
  Matrix.rows model |> Array.map viewRow |> Array.toList |> flow down

viewRow : Array Bool -> Element
viewRow row =
  Array.map viewCell row |> Array.toList |> flow right

viewCell : Bool -> Element
viewCell cell =
  if cell
     then collage 20 20 [(rect 20 20 |> filled Color.black)]
     else collage 20 20 [(rect 20 20 |> outlined (solid Color.black))]

-- Signals

tick : Signal Time
tick =
  Time.every (200 * millisecond)

model : Signal Model
model =
  SignalE.foldp' update initialModel tick

main : Signal Element
main =
  Signal.map view model
