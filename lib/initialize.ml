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


(* Checks if a coordinate is within the grid *)
let in_bounds (r, c) =
  r >= 0 && r < 10 && c >= 0 && c < 10

(* Validates the coordinates of a ship being placed to make sure they are in a line and within the grid*)
let validate_ship_coordinate coords =
  if coords = [] then false
  else if not (List.for_all in_bounds coords) then false
  else
    let sorted = List.sort compare coords in
    let rows = List.map fst sorted in
    let cols = List.map snd sorted in
    if List.for_all ((=) (List.hd rows)) rows then
      let expected =
        List.init (List.length sorted) (fun i -> List.hd cols + i)
      in
      cols = expected
    else if List.for_all ((=) (List.hd cols)) cols then
      let expected =
        List.init (List.length sorted) (fun i -> List.hd rows + i)
      in
      rows = expected
    else false

(* Creates an empty ship list *)
let build_ship_list prefix =
  let names =
    List.map (fun c -> Printf.sprintf "%d%c" prefix c)
      [ 'a'; 'b'; 'c'; 'd'; 'e' ]
  in
  List.map (fun n -> { name = n; coords = CoordSet.empty }) names

(* Places a given ship (ship) on a player's board (board) with the ship's coordinates (coords) *)
let place_ship board ship coords =
  if not (validate_ship_coordinate coords) then
    failwith ("Invalid coordinates for ship" ^ ship.name);
  List.iter
    (fun (r, c) ->
      board.(r).(c) <- SHIP)
    coords;
  ship.coords <- CoordSet.of_list coords

let ship_list0_upd = ref (build_ship_list 0)
let ship_list1_upd = ref (build_ship_list 1)

let ship_list0_og = build_ship_list 0
let ship_list1_og = build_ship_list 1

let board_list =
  [
    initialize_grid ();
    initialize_grid ();
    initialize_grid ();
    initialize_grid ();
  ]
