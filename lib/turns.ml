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

(** [update_board (r,c) player hit_type] changes board of [player] at
    coordinates [(r,c)] to [hit_type]. *)
let update_boards (r, c) (player : int) (hit_type : Initialize.grid_state) =
  let my_attack_board, other_personal_board =
    match player with
    | 0 -> (List.nth Initialize.board_list 1, List.nth Initialize.board_list 2)
    | 1 -> (List.nth Initialize.board_list 3, List.nth Initialize.board_list 0)
    | _ -> failwith "not possible?"
  in
  let attack_row = my_attack_board.(r) in
  attack_row.(c) <- hit_type;
  let personal_row = other_personal_board.(r) in
  personal_row.(c) <- hit_type

(* removes coordinate and returns if ship was hit or sunk and the ship that was
   hit *)
let remove_coord (r, c) (ship_coords : Initialize.ship list) :
    Initialize.grid_state * Initialize.ship =
  failwith "Unimplemented"

let change_to_sink (ship : Initialize.ship)
    (ship_list_og : Initialize.ship list) : unit =
  failwith "Unimplemented"

let check_win (ship_coords : Initialize.ship list) : bool =
  failwith "Unimplemented"

let handle_turn (r, c) (player : int) =
  let other_player, ship_list_upd, ship_list_og =
    match player with
    | 0 -> (1, Initialize.ship_list1_upd, Initialize.ship_list1_og)
    | 1 -> (0, Initialize.ship_list0_upd, Initialize.ship_list0_og)
    | _ -> failwith "impossible player?"
  in
  match validate_coord (r, c) player with
  | false, error, _ -> (error, player)
  | true, _, state ->
      if state = Initialize.SHIP then begin
        let hit_or_sink, ship_hit = remove_coord (r, c) ship_list_upd in
        if hit_or_sink = Initialize.HIT then begin
          update_boards (r, c) player Initialize.HIT;
          ("Hit! Go again.", player)
        end
        else begin
          change_to_sink ship_hit ship_list_og;
          if check_win ship_list_upd = true then
            (Printf.sprintf "Player %s wins!" (string_of_int player), player + 3)
          else ("You sank a ship! Go again.", player)
        end
      end
      else ("Miss", other_player)
