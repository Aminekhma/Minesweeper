module Mine exposing (Mine, Options, generateRandomMines,randomMinesGenerator)

import Random exposing (Generator)
import Set
import Html.Attributes exposing (width)

type alias Mine =
    ( Int )

type alias Options =
    { width : Int
    , height : Int
    , minMines : Int
    , maxMines : Int
    , initialX : Int
    , initialY : Int
    }


randomMinesGeneratorList : Options -> Int -> Generator (List Mine)
randomMinesGeneratorList { width, height } len =
    Random.list len <|
        Random.int 0 width
         


relaunch : Options -> List Mine -> Generator (List Mine)
relaunch ({ minMines, initialX } as options) l =
    if List.length l >= minMines && not (List.member ( initialX ) l) then
        Random.constant l

    else
        randomMinesGenerator options


randomMinesGenerator : Options -> Generator (List Mine)
randomMinesGenerator ({ minMines, maxMines } as options) =
    Random.int minMines maxMines
        |> Random.andThen (randomMinesGeneratorList options)
        |> Random.map (Set.fromList >> Set.toList)
        |> Random.andThen (relaunch options)


generateRandomMines : Options -> (List Mine -> msg) -> Cmd msg
generateRandomMines options msg =
    Random.generate msg (randomMinesGenerator options)
