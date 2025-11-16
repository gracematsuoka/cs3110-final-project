open Cs3110_final_project.Initialize

let player0personal = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |]
let player0attack = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |]
let player1personal = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |]
let player1attack = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |]

let board_list : grid_state array array list =
  [ player0personal; player0attack; player1personal; player1attack ]
