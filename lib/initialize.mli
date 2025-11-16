(* Initialize Interface *)

type ship = {
  name : string;  (** Ship name: "0a"…"0e" or "1a"…"1e". *)
  coords : (int * int) list;  (** List of (row, col) cells occupied. *)
}
(** A ship with a name and its occupied coordinates. *)

type grid_state =
  | EMPTY
  | SHIP
  | HIT
  | MISS
  | SINK

val board_list : grid_state array array list
(** list of boards board_list[0] is player0 personal board; board_list[1] is
    player0 attack board; board_list[2] is player1 personal board; board_list[3]
    is player1 attack board *)
