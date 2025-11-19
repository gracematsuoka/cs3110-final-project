(* Battleship Initialization Module *)

module Coord = struct
  type t = int * int

  let compare = compare
end

(* Set of coordinates used to store ship locations. *)
module CoordSet = Set.Make (Coord)

(* Each ship has a name and a mutable set of coordinates. *)
type ship = {
  name : string;
  mutable coords : CoordSet.t;
}

(* Possible states for each cell on the grid. *)
type grid_state =
  | EMPTY
  | SHIP
  | HIT
  | MISS
  | SINK

(* Initializes grid of size 10x10. *)
let grid_size = 10

(* Creates empty 10x10 grid. *)
let initialize_grid () =
  Array.init grid_size (fun _ ->
      Array.init grid_size (fun _ -> EMPTY))


(* Builds 5 ships for a given player. *)

let in_bounds (r, c) =
  r >= 0 && r < 10 && c >= 0 && c < 10

let validate_ship_coordinate coords =
  coords <> [] &&
  List.for_all in_bounds coords

(* Creates an empty ship list *)
let build_ship_list player =
  let names =
    List.map (fun c -> Printf.sprintf "%d%c" player c)
      [ 'a'; 'b'; 'c'; 'd'; 'e' ]
  in
  List.map (fun n -> { name = n; coords = CoordSet.empty }) names

let place_ship board ship coords =
  if not (validate_ship_coordinate coords) then
    failwith ("Invalid coordinates for ship " ^ ship.name);
  List.iter
    (fun (r, c) ->
      board.(r).(c) <- SHIP)
    coords;
  ship.coords <- List.fold_left (fun set xy -> CoordSet.add xy set) CoordSet.empty coords

let ship_list0_upd = build_ship_list 0
let ship_list1_upd = build_ship_list 1

let ship_list0_og = build_ship_list 0
let ship_list1_og = build_ship_list 1

let board_list =
  [
    initialize_grid ();
    initialize_grid ();
    initialize_grid ();
    initialize_grid ();
  ]
