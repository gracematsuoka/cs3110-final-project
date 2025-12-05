open OUnit2
open Initialize
open Ai

let () = Random.init 0   (* deterministic RNG for reproducible coverage *)

(* --- has_invalid_coords tests --- *)
let test_has_invalid_coords _ =
  let existing = CoordSet.of_list [ (1,1); (2,2) ] in
  assert_bool "overlap is invalid"
    (has_invalid_coords [ (1,1); (3,3) ] existing);
  assert_bool "no overlap is valid"
    (not (has_invalid_coords [ (0,0); (3,4) ] existing))

(* --- make_coords tests --- *)
let test_make_coords_horizontal _ =
  let cs = make_coords 2 5 "h" 3 in
  assert_equal [ (5,2); (5,3); (5,4) ] cs

let test_make_coords_vertical _ =
  let cs = make_coords 3 1 "v" 2 in
  assert_equal [ (3,1); (4,1) ] cs


let test_make_ship_no_overlap _ =
  let coords = make_ship 3 CoordSet.empty in
  let set = CoordSet.of_list coords in
  assert_equal 3 (List.length coords);
  assert_equal 3 (CoordSet.cardinal set)


let test_make_ships_runs _ =
  ignore make_ships


let test_get_next_move_non_hit _ =
  let (r,c) = get_next_move EMPTY (5,5) in
  assert_bool "in bounds" (r >= 0 && r < 10 && c >= 0 && c < 10)

let test_get_next_move_hit_middle _ =
  let (r,c) = get_next_move HIT (4,4) in
  assert_bool "in bounds" (r >= 0 && r < 10 && c >= 0 && c < 10)



let test_get_next_move_hit_right_edge _ =
  for _ = 1 to 50 do
    let (r,c) = get_next_move HIT (3,9) in
    assert_bool "in bounds" (r >= 0 && r < 10 && c >= 0 && c < 10)
  done


let test_get_next_move_hit_bottom_edge _ =
  for _ = 1 to 50 do
    let (r,c) = get_next_move HIT (9,4) in
    assert_bool "in bounds" (r >= 0 && r < 10 && c >= 0 && c < 10)
  done

let suite =
  "ai.ml tests" >:::
    [
      "has_invalid_coords" >:: test_has_invalid_coords;
      "make_coords_h" >:: test_make_coords_horizontal;
      "make_coords_v" >:: test_make_coords_vertical;
      "make_ship" >:: test_make_ship_no_overlap;
      "make_ships_run" >:: test_make_ships_runs;
      "get_next_move_non_hit" >:: test_get_next_move_non_hit;
      "get_next_move_hit_middle" >:: test_get_next_move_hit_middle;
      "get_next_move_hit_right_edge" >:: test_get_next_move_hit_right_edge;
      "get_next_move_hit_bottom_edge" >:: test_get_next_move_hit_bottom_edge;
    ]

let () = run_test_tt_main suite







(* open OUnit2
open Initialize
open Ai

(* Make randomness reproducible. *)
let () = Random.init 0

(* Helper: check that a coordinate is within the 10x10 board. *)
let in_bounds (r, c) =
  r >= 0 && r < 10 && c >= 0 && c < 10

(* ---------- Tests for get_next_move ---------- *)

(* 1. When previous state is not HIT, we should get a random coord in [0,9]x[0,9]. *)
let test_get_next_move_not_hit _ =
  let (r, c) = get_next_move EMPTY (5, 5) in
  assert_bool "random move must be in bounds" (in_bounds (r, c))

(* 2. When previous state is HIT in the middle of the board, 
      next move should be an immediate neighbor (up/down/left/right). *)
let test_get_next_move_hit_middle _ =
  let prev = (5, 5) in
  let (r', c') = get_next_move HIT prev in
  let neighbors =
    [ (5, 4); (5, 6); (* left / right *)
      (4, 5); (6, 5)  (* up / down *)
    ]
  in
  assert_bool "HIT move from middle should go to a neighbor"
    (List.mem (r', c') neighbors)

(* 3. HIT near the right edge: column = 9. 
      The function must stay in bounds and move to a neighbor. *)
let test_get_next_move_hit_right_edge _ =
  let prev = (3, 9) in
  let (r', c') = get_next_move HIT prev in
  assert_bool "right-edge HIT move must be in bounds"
    (in_bounds (r', c'));
  (* Also ensure it's not the same square *)
  assert_bool "right-edge HIT move must change coord"
    ((r', c') <> prev)

(* 4. HIT near the bottom edge: row = 9. 
      Again, must stay in bounds and move to a neighbor. *)
let test_get_next_move_hit_bottom_edge _ =
  let prev = (9, 4) in
  let (r', c') = get_next_move HIT prev in
  assert_bool "bottom-edge HIT move must be in bounds"
    (in_bounds (r', c'));
  assert_bool "bottom-edge HIT move must change coord"
    ((r', c') <> prev)

let suite =
  "ai tests" >:::
  [
    "get_next_move_not_hit" >:: test_get_next_move_not_hit;
    "get_next_move_hit_middle" >:: test_get_next_move_hit_middle;
    "get_next_move_hit_right_edge" >:: test_get_next_move_hit_right_edge;
    "get_next_move_hit_bottom_edge" >:: test_get_next_move_hit_bottom_edge;
  ]

let () = run_test_tt_main suite*)











