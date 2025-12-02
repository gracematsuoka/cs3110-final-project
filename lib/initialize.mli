(* Initialize Interface *)

(** for each element of a board *)
type grid_state =
  | EMPTY
  | SHIP
  | HIT
  | MISS
  | SINK

module Coord : sig
  type t = int * int

  val compare : t -> t -> int
end

module CoordSet : Set.S with type elt = Coord.t

type ship = {
  name : string;  (** Ship name: "0a"…"0e" or "1a"…"1e". *)
  mutable coords : CoordSet.t;
}
(** A ship with a name and its occupied coordinates. *)

val ship_list0_upd : ship list ref
(** list of 5 ships for player0, coordinates update with hits *)

val ship_list1_upd : ship list ref
(** list of 5 ships for player1, coordinates update with hits *)

val ship_list0_og : ship list
(** list of 5 ships for player0, not linked to any part of ship_list0_upd and
    will not change *)

val ship_list1_og : ship list
(** list of 5 ships for player1, not linked to any part of ship_list0_upd and
    will not change *)

val in_bounds : (int * int) -> bool
(** checks if a coordinate is within the 10x10 grid bounds *)

val initialize_grid : unit -> grid_state array array
(** initializes a 10x10 grid with all EMPTY values *)

val validate_ship_coordinate :
  grid_state array array -> (int * int) list -> string
(** checks if the ship coordinates are valid (in bounds and not overlapping) *)

val place_ship: grid_state array array -> ship -> Coord.t list -> unit
val board_list : grid_state array array list
(** list of boards board_list[0] is player0 personal board; board_list[1] is
    player0 attack board; board_list[2] is player1 personal board; board_list[3]
    is player1 attack board *)
