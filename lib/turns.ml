let validate_coord (r, c) player : bool * string * Initialize.grid_state =
  if r > 9 || c > 9 || r < 0 || c < 0 then
    ( false,
      "Coordinates are out of bounds (each input can only be from 0 to 9)",
      Initialize.EMPTY )
  else (true, "", Initialize.SHIP)
(* if player = 0 then let state = Array.get (List.nth Initialize.board_list 1) c
   in if state = EMPTY *)

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
