[@@@coverage exclude_file]

open OUnit2
open Cs3110_final_project.Initialize

let fresh_board () =
  Array.init 10 (fun _ -> Array.init 10 (fun _ -> EMPTY))

let reset_boards () =
  List.iter
    (fun board ->
      Array.iter
        (fun row ->
          for c = 0 to Array.length row - 1 do
            row.(c) <- EMPTY
          done)
        board)
    board_list

let reset_ship_lists () =
  List.iter (fun s -> s.coords <- CoordSet.empty) ship_list0_og;
  List.iter (fun s -> s.coords <- CoordSet.empty) ship_list1_og;
  set_upd_lists ()

let reset_state () =
  reset_boards ();
  reset_ship_lists ()

let test_board_list_shape _ =
  assert_equal 4 (List.length board_list);
  List.iter
    (fun board ->
      assert_equal 10 (Array.length board);
      Array.iter (fun row -> assert_equal 10 (Array.length row)) board)
    board_list

let test_board_list_initial_empty _ =
  List.iter
    (fun board ->
      for r = 0 to 9 do
        for c = 0 to 9 do
          assert_equal EMPTY board.(r).(c)
        done
      done)
    board_list

let test_validate_empty_coords _ =
  let grid = fresh_board () in
  let msg = validate_ship_coordinate grid [] in
  assert_equal "No coordinate entered" msg

let test_validate_out_of_bounds_low _ =
  let grid = fresh_board () in
  let msg = validate_ship_coordinate grid [ (-1, 0) ] in
  assert_equal "Coordinate not in bounds" msg

let test_validate_out_of_bounds_high _ =
  let grid = fresh_board () in
  let msg = validate_ship_coordinate grid [ (10, 0) ] in
  assert_equal "Coordinate not in bounds" msg

let test_validate_not_aligned _ =
  let grid = fresh_board () in
  let msg = validate_ship_coordinate grid [ (0, 0); (1, 1) ] in
  assert_equal "Coordinate must be in same line as this ship" msg

let test_validate_not_consecutive_row _ =
  let grid = fresh_board () in
  let msg = validate_ship_coordinate grid [ (0, 0); (0, 2) ] in
  assert_equal "Coordinate not consecutive to this ship" msg

let test_validate_not_consecutive_col _ =
  let grid = fresh_board () in
  let msg = validate_ship_coordinate grid [ (0, 0); (2, 0) ] in
  assert_equal "Coordinate not consecutive to this ship" msg

let test_validate_overlap_existing_ship _ =
  let grid = fresh_board () in
  grid.(0).(0) <- SHIP;
  let msg = validate_ship_coordinate grid [ (0, 0); (0, 1) ] in
  assert_equal "This coordinate already has a ship" msg

let test_validate_valid_horizontal _ =
  let grid = fresh_board () in
  let coords = [ (1, 1); (1, 2); (1, 3) ] in
  let msg = validate_ship_coordinate grid coords in
  assert_equal "Valid Coordinate" msg

let test_validate_valid_vertical _ =
  let grid = fresh_board () in
  let coords = [ (2, 5); (3, 5); (4, 5) ] in
  let msg = validate_ship_coordinate grid coords in
  assert_equal "Valid Coordinate" msg

let test_place_ship_success_updates_board_and_ship _ =
  reset_state ();
  let board = List.nth board_list 0 in
  let ship = List.hd ship_list0_og in
  let coords = [ (3, 3); (3, 4); (3, 5) ] in
  place_ship board ship coords;
  List.iter
    (fun (r, c) ->
      assert_equal SHIP board.(r).(c);
      assert_bool "ship.coords contains coord"
        (CoordSet.mem (r, c) ship.coords))
    coords

let test_place_ship_invalid_raises _ =
  reset_state ();
  let board = List.nth board_list 0 in
  let ship = List.hd ship_list0_og in
  let coords = [ (-1, 0) ] in
  let f () = place_ship board ship coords in
  assert_raises
    (Failure "Invalid coordinates for ship 0a. Coordinate not in bounds")
    f

let test_ship_list_names_and_lengths _ =
  reset_state ();
  let names0 = List.map (fun s -> s.name) ship_list0_og in
  let names1 = List.map (fun s -> s.name) ship_list1_og in
  assert_equal 5 (List.length names0);
  assert_equal 5 (List.length names1);
  assert_equal [ "0a"; "0b"; "0c"; "0d"; "0e" ] names0;
  assert_equal [ "1a"; "1b"; "1c"; "1d"; "1e" ] names1

let test_no_coord_entered _ =
  let result = validate_ship_coordinate (fresh_board ()) [] in
  assert_equal "No coordinate entered" result
let test_ship_lists_initial_coords_empty _ =
  reset_state ();
  let all_empty ships =
    List.for_all (fun s -> CoordSet.is_empty s.coords) ships
  in
  assert_bool "p0 og empty" (all_empty ship_list0_og);
  assert_bool "p1 og empty" (all_empty ship_list1_og);
  assert_bool "p0 upd empty" (all_empty !ship_list0_upd);
  assert_bool "p1 upd empty" (all_empty !ship_list1_upd)

let test_set_upd_lists_makes_independent_copies _ =
  reset_state ();
  let ship0_og = List.hd ship_list0_og in
  ship0_og.coords <- CoordSet.add (0, 0) CoordSet.empty;
  set_upd_lists ();
  let ship0_upd = List.hd !ship_list0_upd in
  assert_equal ship0_og.name ship0_upd.name;
  ship0_upd.coords <- CoordSet.add (1, 1) CoordSet.empty;
  assert_bool "og still has (0,0)"
    (CoordSet.mem (0, 0) ship0_og.coords);
  assert_bool "og does not have (1,1)"
    (not (CoordSet.mem (1, 1) ship0_og.coords))

let test_board_list_boards_independent _ =
  reset_state ();
  let b0 = List.nth board_list 0 in
  let b1 = List.nth board_list 1 in
  b0.(2).(2) <- SHIP;
  assert_equal SHIP b0.(2).(2);
  assert_equal EMPTY b1.(2).(2)

let tests =
  [
    "board_list_shape" >:: test_board_list_shape;
    "board_list_initial_empty" >:: test_board_list_initial_empty;
    "validate_empty_coords" >:: test_validate_empty_coords;
    "validate_oob_low" >:: test_validate_out_of_bounds_low;
    "validate_oob_high" >:: test_validate_out_of_bounds_high;
    "validate_not_aligned" >:: test_validate_not_aligned;
    "validate_not_consecutive_row" >:: test_validate_not_consecutive_row;
    "validate_not_consecutive_col" >:: test_validate_not_consecutive_col;
    "validate_overlap_existing_ship" >:: test_validate_overlap_existing_ship;
    "validate_valid_horizontal" >:: test_validate_valid_horizontal;
    "validate_valid_vertical" >:: test_validate_valid_vertical;
    "place_ship_success" >:: test_place_ship_success_updates_board_and_ship;
    "place_ship_invalid_raises" >:: test_place_ship_invalid_raises;
    "ship_list_names_and_lengths" >:: test_ship_list_names_and_lengths;
    "ship_lists_initial_coords_empty" >:: test_ship_lists_initial_coords_empty;
    "set_upd_lists_independent" >:: test_set_upd_lists_makes_independent_copies;
    "board_list_boards_independent" >:: test_board_list_boards_independent;
    "no_coord_entered" >:: test_no_coord_entered;
  ]

let test_suite = "initialize test suite" >::: tests
let _ = run_test_tt_main test_suite