let validate_coord (r, c) player : bool * string * Initialize.grid_state =
  if r > 9 || c > 9 || r < 0 || c < 0 then
    ( false,
      "Coordinates are out of bounds (each input can only be from 0 to 9)",
      Initialize.EMPTY )
  else
    let attack_board =
      List.nth Initialize.board_list (if player = 0 then 1 else 0)
    in
    let ship_list_upd =
      if player = 0 then Initialize.ship_list1 else Initialize.ship_list0
    in
    let state = Array.get (Array.get attack_board c) r in
    if state <> Initialize.EMPTY then
      ( false,
        "Enter a coordinate that has not already been entered",
        Initialize.EMPTY )
    else
      let is_ship =
        List.exists
          (fun s -> Initialize.CoordSet.mem (r, c) s.coords)
          ship_list_upd
      in
      (* CHECK IF SUNK SHIP *)
      if is_ship then (true, "", Initialize.HIT) else (true, "", Initialize.MISS)

let update_personal_board (r, c) (hit_type : Initialize.grid_state) =
  failwith "Unimplemented"

let update_attack_board (r, c) (hit_type : Initialize.grid_state) =
  failwith "Unimplemented"

let remove_coord (r, c) ship_coords = failwith "Unimplemented"
let check_win ship_coords = failwith "Unimplemented"

let handle_turn (r, c) player =
  match validate_coord (r, c) player with
  | false, error, _ -> (error, player)
  | true, _, state -> ("TO IMPLEMENT", player)
