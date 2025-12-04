open Initialize
(* AI is player1 *)

type input = {
  mutable state : Initialize.grid_state;
  mutable coord : int * int;
}

let last_input = { state = EMPTY; coord = (0, 0) }
let personal_board = List.nth board_list 2

(** [has_invalid_coords] checks if any coord in [coords] is already in
    [existing_coords] *)
let has_invalid_coords coords existing_coords =
  List.exists (fun coord -> CoordSet.mem coord existing_coords) coords

(** [make_coords] creates a list of coords of length [slots] given the [fixed]
    and [start] coord and the orientation [orient] of the ship *)
let make_coords fixed start orient slots =
  List.init slots (fun i ->
      if orient = "h" then (start, fixed + i) else (fixed + i, start))

(** [make_ship] creates a valid list of coords (doesn't overlap with
    [existing coords] for a given ship with some number of [slots] *)
let rec make_ship slots existing_coords =
  let orient = if Random.int 2 = 0 then "h" else "v" in
  let coords =
    make_coords (Random.int (11 - slots)) (Random.int 10) orient slots
  in
  if has_invalid_coords coords existing_coords then
    make_ship slots existing_coords
  else coords

let make_ships =
  let rec add_ships ship_num existing_coords =
    match ship_num with
    | 0 -> ()
    | _ ->
        let coords = make_ship ship_num existing_coords in
        let new_existing_coords =
          List.fold_left
            (fun acc coord -> CoordSet.add coord acc)
            existing_coords coords
        in
        let ship = List.nth ship_list1_og (ship_num - 1) in
        place_ship personal_board ship coords;
        ship.coords <- CoordSet.of_list coords;
        add_ships (ship_num - 1) new_existing_coords
  in
  add_ships 3 CoordSet.empty

let get_next_move (prev_state : grid_state) (prev_input : int * int) : int * int
    =
  match prev_state with
  | HIT ->
      let dir = if Random.int 2 = 0 then "r" else "d" in
      let r = fst prev_input in
      let c = snd prev_input in
      if dir = "r" then if c >= 9 then (r, c - 1) else (r, c + 1)
      else if r >= 9 then (r - 1, c)
      else (r + 1, c)
  | _ -> (Random.int 10, Random.int 10)
