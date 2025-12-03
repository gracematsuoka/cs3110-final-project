(* Turns Interface *)

val handle_turn : int * int -> int -> string * int * (int * int) list
(** [handle_turn (r,c) player] processes a move where [player] attacks
    coordinate [(r,c)].

    - If [(r,c)] is out of bounds or has already been attacked, the move is
      invalid and [player] goes again.
    - If [(r,c)] is EMPTY on the other player's personal board, the attack is a
      MISS and the other player goes next.
    - If [(r,c)] is SHIP on other player's personal board, the attack is a HIT
      or a SINK: if the other player still has remaining ships, [player] foes
      again; if the other player has no ships left, [player] wins.

    Requires [player] is 0 or 1.

    Returns message describing result of attack and and next player who should
    take a turn -- 0 means player 0 goes next, 1 means player 1 goes next, 3
    means player 0 wins, 4 means player 1 wins. *)
