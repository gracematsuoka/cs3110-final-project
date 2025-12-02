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
  Array.init grid_size (fun _ -> Array.init grid_size (fun _ -> EMPTY))

(* Builds 5 ships for a given player. *)

let in_bounds (r, c) = r >= 0 && r < 10 && c >= 0 && c < 10

let validate_ship_coordinate (grid : grid_state array array)
    (coords : (int * int) list) =
  match coords with
  | [] -> false
  | (r0, c0) :: _ ->
      if not (List.for_all in_bounds coords) then false
      else
        let same_row = List.for_all (fun (r, _) -> r = r0) coords in
        let same_col = List.for_all (fun (_, c) -> c = c0) coords in
        if not (same_row || same_col) then false
        else
          let sorted =
            if same_row then
              List.sort (fun (_, c1) (_, c2) -> compare c1 c2) coords
            else List.sort (fun (r1, _) (r2, _) -> compare r1 r2) coords
          in

          let rec consecutive = function
            | [] | [ _ ] -> true
            | (r1, c1) :: (r2, c2) :: rest ->
                let good = if same_row then c2 = c1 + 1 else r2 = r1 + 1 in
                good && consecutive ((r2, c2) :: rest)
          in
          if not (consecutive sorted) then false
          else
            List.for_all
              (fun (r, c) ->
                match grid.(r).(c) with
                | SHIP -> false
                | _ -> true)
              coords

(* Creates an empty ship list ref*)
let build_ship_list player =
  let names =
    List.map
      (fun c -> Printf.sprintf "%d%c" player c)
      [ 'a'; 'b'; 'c'; 'd'; 'e' ]
  in
  List.map (fun n -> { name = n; coords = CoordSet.empty }) names

let place_ship board ship coords =
  if not (validate_ship_coordinate board coords) then
    failwith ("Invalid coordinates for ship " ^ ship.name);
  List.iter (fun (r, c) -> board.(r).(c) <- SHIP) coords;
  ship.coords <-
    List.fold_left (fun set xy -> CoordSet.add xy set) CoordSet.empty coords

let ship_list0_og = build_ship_list 0
let ship_list1_og = build_ship_list 1
let ship_list0_upd = ref ship_list0_og
let ship_list1_upd = ref ship_list1_og

let board_list =
  [
    initialize_grid ();
    initialize_grid ();
    initialize_grid ();
    initialize_grid ();
  ]
