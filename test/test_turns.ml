open OUnit2
open Batteries
open Cs3110_final_project.Turns
open Cs3110_final_project.Initialize

let reset_game_state () =
  List.iter
    (fun board ->
      Array.iter (fun row -> Array.fill row 0 (Array.length row) EMPTY) board)
    board_list;

  List.iter (fun s -> s.coords <- CoordSet.empty) ship_list0_og;
  List.iter (fun s -> s.coords <- CoordSet.empty) ship_list1_og;
  ()

let find_ship_by_name name ship_list =
  List.find (fun s -> s.name = name) ship_list

let read_csv_and_set_board (filename : string) : unit =
  (* ChatGPT *)
  reset_game_state ();
  let lines = List.of_enum (File.lines_of filename) in
  match lines with
  | [] | [ _ ] -> failwith "CSV must have header + at least one data row"
  | _header :: rows ->
      List.iter
        (fun line ->
          if String.trim line <> "" then begin
            let parts = String.split_on_char ',' line in
            match parts with
            | [ player_str; ship_name; coords_str ] ->
                let player =
                  try int_of_string (String.trim player_str)
                  with _ -> failwith "Invalid player number in CSV"
                in
                let coords =
                  if String.trim coords_str = "" then []
                  else
                    List.map
                      (fun s ->
                        match String.split_on_char '-' s with
                        | [ r; c ] -> (int_of_string r, int_of_string c)
                        | _ ->
                            failwith ("Bad coord " ^ s ^ " in CSV: must be r-c"))
                      (String.split_on_char ';' (String.trim coords_str))
                in
                let personal_board =
                  if player = 0 then List.nth board_list 0
                  else if player = 1 then List.nth board_list 2
                  else failwith "Player must be 0 or 1 in CSV"
                in
                let ship =
                  if player = 0 then find_ship_by_name ship_name ship_list0_og
                  else find_ship_by_name ship_name ship_list1_og
                in
                ship.coords <-
                  List.fold_left
                    (fun acc (r, c) -> CoordSet.add (r, c) acc)
                    CoordSet.empty coords;
                place_ship personal_board ship coords
            | _ -> failwith ("Malformed CSV line: " ^ line)
          end)
        rows;
      set_upd_lists ();
      ()

let next_player (triple : string * int * (int * int) list) : int =
  match triple with
  | _, mid, _ -> mid

let tests =
  [
    ("template" >:: fun _ -> assert_equal 0 0);
    ( "invalid coordinate out of bounds, row, col < 0" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual : string * int * (int * int) list = handle_turn (-1, -1) 0 in
      let expected =
        ( "Coordinates are out of bounds (each input can only be from 0 to 9)",
          0,
          [] )
      in
      assert_equal actual expected );
    ( "invalid coordinate out of bounds, row, col > 9" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual : string * int * (int * int) list = handle_turn (10, 10) 0 in
      let expected =
        ( "Coordinates are out of bounds (each input can only be from 0 to 9)",
          0,
          [] )
      in
      assert_equal actual expected );
    ( "invalid coordinate out of bounds, row < 0" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual : string * int * (int * int) list = handle_turn (-1, 0) 0 in
      let expected =
        ( "Coordinates are out of bounds (each input can only be from 0 to 9)",
          0,
          [] )
      in
      assert_equal actual expected );
    ( "invalid coordinate out of bounds, col < 0" >:: fun _ ->
      let actual : string * int * (int * int) list = handle_turn (0, -1) 0 in
      let expected =
        ( "Coordinates are out of bounds (each input can only be from 0 to 9)",
          0,
          [] )
      in
      assert_equal actual expected );
    ( "invalid coordinate out of bounds, row > 9" >:: fun _ ->
      let actual : string * int * (int * int) list = handle_turn (10, 0) 1 in
      let expected =
        ( "Coordinates are out of bounds (each input can only be from 0 to 9)",
          1,
          [] )
      in
      assert_equal actual expected );
    ( "invalid coordinate out of bounds, col > 9" >:: fun _ ->
      let actual : string * int * (int * int) list = handle_turn (0, 10) 1 in
      let expected =
        ( "Coordinates are out of bounds (each input can only be from 0 to 9)",
          1,
          [] )
      in
      assert_equal actual expected );
    ( "player 0 already entered coord, miss" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (9, 0) 0);
      assert_equal (List.nth board_list 1).(9).(0) MISS;
      let actual = handle_turn (9, 0) 0 in
      let expected =
        ("Enter a coordinate that has not already been entered", 0, [])
      in
      assert_equal expected actual );
    ( "player 0 already entered coord, hit" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (9, 9) 0);
      assert_equal (List.nth board_list 1).(9).(9) HIT;

      let actual = handle_turn (9, 9) 0 in
      let expected =
        ("Enter a coordinate that has not already been entered", 0, [])
      in
      assert_equal expected actual );
    ( "player 0 already entered coord, sink" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (5, 9) 0);
      ignore (handle_turn (5, 8) 0);
      assert_equal (List.nth board_list 1).(5).(9) SINK;
      assert_equal (List.nth board_list 1).(5).(8) SINK;

      let actual = handle_turn (5, 9) 0 in
      let expected =
        ("Enter a coordinate that has not already been entered", 0, [])
      in
      assert_equal expected actual );
    ( "player 1 already entered coord, miss" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (0, 9) 1);
      assert_equal (List.nth board_list 3).(0).(9) MISS;
      let actual = handle_turn (0, 9) 1 in
      let expected =
        ("Enter a coordinate that has not already been entered", 1, [])
      in
      assert_equal expected actual );
    ( "player 1 already entered coord, hit" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (0, 0) 1);
      assert_equal (List.nth board_list 3).(0).(0) HIT;

      let actual = handle_turn (0, 0) 1 in
      let expected =
        ("Enter a coordinate that has not already been entered", 1, [])
      in
      assert_equal expected actual );
    ( "player 1 already entered coord, sink" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (4, 0) 1);
      ignore (handle_turn (4, 1) 1);
      assert_equal (List.nth board_list 3).(4).(0) SINK;
      assert_equal (List.nth board_list 3).(4).(1) SINK;

      let actual = handle_turn (4, 0) 1 in
      let expected =
        ("Enter a coordinate that has not already been entered", 1, [])
      in
      assert_equal expected actual );
    ( "invalid player" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let f = fun () -> handle_turn (0, 0) 3 in
      assert_raises (Invalid_argument "Player must be 0 or 1") f );
    ( "player 0 miss" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual = handle_turn (9, 4) 0 in
      let expected = ("Miss", 1, []) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 1).(9).(4) MISS;
      assert_equal (List.nth board_list 2).(9).(4) MISS );
    ( "player 0 hit" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual = handle_turn (9, 9) 0 in
      let expected = ("Hit! Go again.", 0, []) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 1).(9).(9) HIT;
      assert_equal (List.nth board_list 2).(9).(9) HIT );
    ( "player 0 hit multiple, not sink" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (9, 9) 0);
      ignore (handle_turn (9, 7) 0);
      let actual = handle_turn (9, 5) 0 in
      let expected = ("Hit! Go again.", 0, []) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 1).(9).(9) HIT;
      assert_equal (List.nth board_list 2).(9).(9) HIT;
      assert_equal (List.nth board_list 1).(9).(7) HIT;
      assert_equal (List.nth board_list 2).(9).(7) HIT;
      assert_equal (List.nth board_list 1).(9).(5) HIT;
      assert_equal (List.nth board_list 2).(9).(5) HIT );
    ( "player 0 hit, sink" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (5, 9) 0);
      let actual = handle_turn (5, 8) 0 in
      let expected = ("You sank a ship! Go again.", 0, [ (5, 8); (5, 9) ]) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 1).(5).(8) SINK;
      assert_equal (List.nth board_list 2).(5).(9) SINK );
    ( "player 1 miss" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual = handle_turn (0, 5) 1 in
      let expected = ("Miss", 0, []) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 0).(0).(5) MISS;
      assert_equal (List.nth board_list 3).(0).(5) MISS );
    ( "player 1 hit" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      let actual = handle_turn (0, 0) 1 in
      let expected = ("Hit! Go again.", 1, []) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 0).(0).(0) HIT;
      assert_equal (List.nth board_list 3).(0).(0) HIT );
    ( "player 1 hit multiple, not sink" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (0, 0) 1);
      ignore (handle_turn (0, 2) 1);
      let actual = handle_turn (0, 4) 1 in
      let expected = ("Hit! Go again.", 1, []) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 0).(0).(0) HIT;
      assert_equal (List.nth board_list 3).(0).(0) HIT;
      assert_equal (List.nth board_list 0).(0).(2) HIT;
      assert_equal (List.nth board_list 3).(0).(2) HIT;
      assert_equal (List.nth board_list 0).(0).(4) HIT;
      assert_equal (List.nth board_list 3).(0).(4) HIT );
    ( "player 1 hit, sink" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (4, 0) 1);
      let actual = handle_turn (4, 1) 1 in
      let expected = ("You sank a ship! Go again.", 1, [ (4, 0); (4, 1) ]) in
      assert_equal expected actual;
      assert_equal (List.nth board_list 0).(4).(0) SINK;
      assert_equal (List.nth board_list 3).(4).(1) SINK );
    ( "player 0 wins" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (9, 9) 0);
      ignore (handle_turn (9, 8) 0);
      ignore (handle_turn (9, 7) 0);
      ignore (handle_turn (9, 6) 0);
      ignore (handle_turn (9, 5) 0);
      ignore (handle_turn (8, 9) 0);
      ignore (handle_turn (8, 8) 0);
      ignore (handle_turn (8, 7) 0);
      ignore (handle_turn (8, 6) 0);
      ignore (handle_turn (7, 9) 0);
      ignore (handle_turn (7, 8) 0);
      ignore (handle_turn (7, 7) 0);
      ignore (handle_turn (6, 9) 0);
      ignore (handle_turn (6, 8) 0);
      ignore (handle_turn (6, 7) 0);
      ignore (handle_turn (5, 9) 0);
      let actual = handle_turn (5, 8) 0 in
      let sunk_ship = [ (5, 8); (5, 9) ] in
      let expected = ("Player 0 wins!", 3, sunk_ship) in
      assert_equal expected actual );
    ( "player 1 wins" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      ignore (handle_turn (0, 0) 1);
      ignore (handle_turn (0, 1) 1);
      ignore (handle_turn (0, 2) 1);
      ignore (handle_turn (0, 3) 1);
      ignore (handle_turn (0, 4) 1);
      ignore (handle_turn (1, 0) 1);
      ignore (handle_turn (1, 1) 1);
      ignore (handle_turn (1, 2) 1);
      ignore (handle_turn (1, 3) 1);
      ignore (handle_turn (2, 0) 1);
      ignore (handle_turn (2, 1) 1);
      ignore (handle_turn (2, 2) 1);
      ignore (handle_turn (3, 0) 1);
      ignore (handle_turn (3, 1) 1);
      ignore (handle_turn (3, 2) 1);
      ignore (handle_turn (4, 0) 1);
      let actual = handle_turn (4, 1) 1 in
      let sunk_ship = [ (4, 0); (4, 1) ] in
      let expected = ("Player 1 wins!", 4, sunk_ship) in
      assert_equal expected actual );
    ( "take turns" >:: fun _ ->
      read_csv_and_set_board "../data/ships1.csv";
      (* player 0 miss, player 1 next *)
      assert_equal (next_player (handle_turn (5, 5) 0)) 1;
      (* player 1 miss, player 0 next *)
      assert_equal (next_player (handle_turn (4, 4) 1)) 0;
      (* player 0 sink ship, hit 2 others, none sink, then miss *)
      assert_equal (next_player (handle_turn (7, 9) 0)) 0;
      assert_equal (next_player (handle_turn (7, 8) 0)) 0;
      assert_equal (next_player (handle_turn (7, 7) 0)) 0;
      assert_equal (next_player (handle_turn (9, 8) 0)) 0;
      assert_equal (next_player (handle_turn (9, 7) 0)) 0;
      assert_equal (next_player (handle_turn (8, 7) 0)) 0;
      assert_equal (next_player (handle_turn (4, 5) 0)) 1;
      (* player 1 hit diff ships, none sink, then miss *)
      assert_equal (next_player (handle_turn (1, 0) 1)) 1;
      assert_equal (next_player (handle_turn (1, 1) 1)) 1;
      assert_equal (next_player (handle_turn (1, 2) 1)) 1;
      assert_equal (next_player (handle_turn (2, 2) 1)) 1;
      assert_equal (next_player (handle_turn (2, 0) 1)) 1;
      assert_equal (next_player (handle_turn (5, 0) 1)) 0;
      (* player 0 sink the rest (some already hit, player 0 go again) *)
      assert_equal (next_player (handle_turn (6, 9) 0)) 0;
      assert_equal (next_player (handle_turn (6, 8) 0)) 0;
      assert_equal (next_player (handle_turn (6, 7) 0)) 0;
      assert_equal (next_player (handle_turn (5, 9) 0)) 0;
      assert_equal (next_player (handle_turn (5, 8) 0)) 0;
      assert_equal (next_player (handle_turn (8, 9) 0)) 0;
      assert_equal (next_player (handle_turn (8, 8) 0)) 0;
      assert_equal (next_player (handle_turn (8, 7) 0)) 0;
      assert_equal (next_player (handle_turn (8, 6) 0)) 0;
      assert_equal (next_player (handle_turn (9, 9) 0)) 0;
      assert_equal (next_player (handle_turn (9, 8) 0)) 0;
      assert_equal (next_player (handle_turn (9, 7) 0)) 0;
      assert_equal (next_player (handle_turn (9, 6) 0)) 0;
      let actual = handle_turn (9, 5) 0 in
      let sunk_ship = [ (9, 5); (9, 6); (9, 7); (9, 8); (9, 9) ] in
      let expected = ("Player 0 wins!", 3, sunk_ship) in
      assert_equal expected actual;
      let p0_atk_board = List.nth board_list 1 in
      let p1_pers_board = List.nth board_list 2 in
      for r = 0 to 9 do
        for c = 0 to 9 do
          assert (p0_atk_board.(r).(c) <> SHIP);
          assert (p1_pers_board.(r).(c) <> SHIP)
        done
      done );
  ]

let test_suite = "turns test suite" >::: tests
let _ = run_test_tt_main test_suite
