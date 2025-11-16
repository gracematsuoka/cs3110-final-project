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
