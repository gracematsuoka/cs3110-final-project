open OUnit2
open Initialize
open Ai

let () = Random.init 0  
let test_has_invalid_coords _ =
  let existing = CoordSet.of_list [ (1,1); (2,2) ] in
  assert_bool "overlap is invalid"
    (has_invalid_coords [ (1,1); (3,3) ] existing);
  assert_bool "no overlap is valid"
    (not (has_invalid_coords [ (0,0); (3,4) ] existing))
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
















