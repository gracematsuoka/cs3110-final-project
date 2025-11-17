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
let create_grid () =
  Array.init grid_size (fun _ ->
      Array.init grid_size (fun _ -> EMPTY))


(* Builds 5 ships for a given player. *)
let build_ship_list player =
  let prefix = if player = 0 then "0" else "1" in
  let letters = [ "a"; "b"; "c"; "d"; "e" ] in
  List.map (fun l -> { name = prefix ^ l; coords = CoordSet.empty }) letters


let ship_list0_upd = build_ship_list 0
let ship_list1_upd = build_ship_list 1

let ship_list0_og = build_ship_list 0
let ship_list1_og = build_ship_list 1

let board_list =
  [
    create_grid ();
    create_grid ();
    create_grid ();
    create_grid ();
  ]
