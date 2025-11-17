(** [validate_coord] checks if coordinate [(r,c)] is a valid coordinate that
    [player] entered *)
let validate_coord (r, c) player :
    bool * string * Initialize.grid_state * Initialize.ship =
  let empty_ship : Initialize.ship =
    { name = ""; coords = Initialize.CoordSet.empty }
  in
  if r > 9 || c > 9 || r < 0 || c < 0 then
    ( false,
      "Coordinates are out of bounds (each input can only be from 0 to 9)",
      Initialize.EMPTY,
      empty_ship )
  else
    let attack_board =
      List.nth Initialize.board_list (if player = 0 then 1 else 0)
    in
    let ship_list_upd : Initialize.ship list =
      if player = 0 then Initialize.ship_list1_upd
      else Initialize.ship_list0_upd
    in
    let state = Array.get (Array.get attack_board c) r in
    if state <> Initialize.EMPTY then
      ( false,
        "Enter a coordinate that has not already been entered",
        Initialize.EMPTY,
        empty_ship )
    else
      let target_ship_opt =
        List.find_opt
          (fun (s : Initialize.ship) -> Initialize.CoordSet.mem (r, c) s.coords)
          ship_list_upd
      in
      match target_ship_opt with
      | None -> (true, "", Initialize.EMPTY, empty_ship)
      | Some ship -> (true, "", Initialize.SHIP, ship)

(** [update_boards (r,c) player hit_type] updates the attack board ot [player]
    and the personal board of the other player. For both boards, the grid_state
    at index [(r,c)] is updated to [hit_type]. Requires that [player] is 0 or 1
    and [hit_type] is MISS, HIT, or SINK. *)
let update_boards (r, c) (player : int) (hit_type : Initialize.grid_state) =
  if
    hit_type <> Initialize.MISS
    || hit_type <> Initialize.HIT
    || hit_type <> Initialize.SINK
  then invalid_arg "update_boards: hit_type must be MISS, HIT, or SINK";

  let my_attack_board, other_personal_board =
    match player with
    | 0 -> (List.nth Initialize.board_list 1, List.nth Initialize.board_list 2)
    | 1 -> (List.nth Initialize.board_list 3, List.nth Initialize.board_list 0)
    | _ -> invalid_arg "update_boards: player must be 0 or 1"
  in
  let attack_row = my_attack_board.(r) in
  attack_row.(c) <- hit_type;
  let personal_row = other_personal_board.(r) in
  personal_row.(c) <- hit_type

(** [remove_coord] removes coordinate [(r,c)] from the list of coordinates of
    [ship] *)
let remove_coord (r, c) (ship : Initialize.ship) :
    Initialize.grid_state * string =
  ship.coords <- Initialize.CoordSet.remove (r, c) ship.coords;
  if Initialize.CoordSet.is_empty ship.coords then (Initialize.SINK, ship.name)
  else (Initialize.HIT, ship.name)

(** [change_to_sink ship_name ship_list_og player] changes all HITs to SINKs for
    ship with [ship_name] in [player]'s attack board and other player's personal
    board. Throws [Not_found] if [ship_name] is not the name of a ship in
    [ship_list_og]. *)
let change_to_sink (ship_name : string) (ship_list_og : Initialize.ship list)
    (player : int) : unit =
  let ship =
    List.find (fun (s : Initialize.ship) -> s.name = ship_name) ship_list_og
  in
  let coords = ship.coords in
  Initialize.CoordSet.iter
    (fun (r, c) -> update_boards (r, c) player Initialize.SINK)
    coords

(** [check_win] checks if all coords in [ship_coords] are empty *)
let check_win (ship_coords : Initialize.ship list) : bool =
  List.for_all
    (fun (s : Initialize.ship) -> Initialize.CoordSet.is_empty s.coords)
    ship_coords

let handle_turn (r, c) (player : int) =
  let other_player, ship_list_upd, ship_list_og =
    match player with
    | 0 -> (1, Initialize.ship_list1_upd, Initialize.ship_list1_og)
    | 1 -> (0, Initialize.ship_list0_upd, Initialize.ship_list0_og)
    | _ -> failwith "impossible player?"
  in
  match validate_coord (r, c) player with
  | false, error, _, _ -> (error, player)
  | true, _, state, ship ->
      if state = Initialize.SHIP then begin
        let hit_or_sink, ship_hit_name = remove_coord (r, c) ship in
        if hit_or_sink = Initialize.HIT then begin
          update_boards (r, c) player Initialize.HIT;
          ("Hit! Go again.", player)
        end
        else begin
          change_to_sink ship_hit_name ship_list_og player;
          if check_win ship_list_upd then
            (Printf.sprintf "Player %s wins!" (string_of_int player), player + 3)
          else ("You sank a ship! Go again.", player)
        end
      end
      else ("Miss", other_player)
