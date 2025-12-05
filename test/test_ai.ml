[@@@coverage exclude_file]

open OUnit2
open Cs3110_final_project.Initialize
open Cs3110_final_project.Ai


let () = Random.init 0

let in_bounds (r, c) =
  r >= 0 && r < 10 && c >= 0 && c < 10

let manhattan (r1, c1) (r2, c2) =
  abs (r1 - r2) + abs (c1 - c2)

let test_last_input_initial _ =
  assert_equal EMPTY last_input.state;
  assert_equal (0, 0) last_input.coord

let test_get_next_move_hit_center _ =
  let prev = (5, 5) in
  let next = get_next_move HIT prev in
  assert_bool "" (in_bounds next);
  assert_equal 1 (manhattan prev next)

let test_get_next_move_hit_right_and_down _ =
  let prev = (3, 9) in
  let saw_left = ref false in
  let saw_down = ref false in
  for _ = 1 to 200 do
    let next = get_next_move HIT prev in
    assert_bool "" (in_bounds next);
    if next = (3, 8) then saw_left := true;
    if next = (4, 9) then saw_down := true
  done;
  assert_bool "" !saw_left;
  assert_bool "" !saw_down

let test_get_next_move_hit_up_and_right _ =
  let prev = (9, 4) in
  let saw_up = ref false in
  let saw_right = ref false in
  for _ = 1 to 200 do
    let next = get_next_move HIT prev in
    assert_bool "" (in_bounds next);
    if next = (8, 4) then saw_up := true;
    if next = (9, 5) then saw_right := true
  done;
  assert_bool "" !saw_up;
  assert_bool "" !saw_right

let test_get_next_move_miss_random _ =
  let prev = (5, 5) in
  let seen_different = ref false in
  let seen_positions = ref CoordSet.empty in
  for _ = 1 to 200 do
    let next = get_next_move EMPTY prev in
    assert_bool "" (in_bounds next);
    if next <> prev then seen_different := true;
    seen_positions := CoordSet.add next !seen_positions
  done;
  assert_bool "" !seen_different;
  assert_bool "" (CoordSet.cardinal !seen_positions > 1)

let test_make_ships_does_not_crash _ =
  ignore make_ships

let suite =
  "ai tests" >:::
    [
      "last_input_initial" >:: test_last_input_initial;
      "get_next_move_hit_center" >:: test_get_next_move_hit_center;
      "get_next_move_hit_right_and_down" >:: test_get_next_move_hit_right_and_down;
      "get_next_move_hit_up_and_right" >:: test_get_next_move_hit_up_and_right;
      "get_next_move_miss_random" >:: test_get_next_move_miss_random;
      "make_ships_does_not_crash" >:: test_make_ships_does_not_crash;
    ]

let () = run_test_tt_main suite