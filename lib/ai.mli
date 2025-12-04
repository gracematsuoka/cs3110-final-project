val make_ships : unit
(** [make_ships] randomly creates 3 ships and places them on the ai bot's board
    and ship list *)

val get_next_move : Initialize.grid_state -> int * int -> int * int
(** [get_next_move] returns the coordinate that the AI wants to input as its
    next turn *)

type input = {
  mutable state : Initialize.grid_state;
  mutable coord : int * int;
}

(* keeps track of the last input that the AI guessed and the result of that
   input (grid_state) *)
val last_input : input
