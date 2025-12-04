open Gtk
open Cs3110_final_project.Initialize

(* ---------------------------------------------------------- *)
(*  Emoji + terminal helpers (still available for debugging)  *)
(* ---------------------------------------------------------- *)

let emoji_of_cell = function
  | EMPTY -> "🌊"
  | SHIP -> "🚢"
  | HIT -> "💣"
  | MISS -> "❌"
  | SINK -> " ☠️ "

let print_two_boards board_left board_right : unit Lwt.t =
  let nrows = Array.length board_left in
  let ncols = Array.length board_left.(0) in

  let%lwt () = Lwt_io.print "      " in
  let rec print_cols j =
    if j = ncols then Lwt.return_unit
    else
      let%lwt () = Lwt_io.printf "%d  " j in
      print_cols (j + 1)
  in
  let%lwt () = print_cols 0 in
  let%lwt () = Lwt_io.print "      " in
  let%lwt () = print_cols 0 in
  let%lwt () = Lwt_io.print "\n" in

  let rec print_rows i =
    if i = nrows then Lwt.return_unit
    else
      let%lwt () = Lwt_io.printf "%2d   " i in
      let rec print_left j =
        if j = ncols then Lwt.return_unit
        else
          let%lwt () = Lwt_io.print (emoji_of_cell board_left.(i).(j) ^ " ") in
          print_left (j + 1)
      in
      let%lwt () = print_left 0 in
      let%lwt () = Lwt_io.print "   |   " in
      let rec print_right j =
        if j = ncols then Lwt.return_unit
        else
          let%lwt () = Lwt_io.print (emoji_of_cell board_right.(i).(j) ^ " ") in
          print_right (j + 1)
      in
      let%lwt () = print_right 0 in
      let%lwt () = Lwt_io.print "\n" in
      print_rows (i + 1)
  in
  let%lwt () = print_rows 0 in
  Lwt_io.print "\n"

let print_board board =
  let nrows = Array.length board in
  let ncols = Array.length board.(0) in
  Printf.printf "    ";
  for j = 0 to ncols - 1 do
    Printf.printf "%d  " j
  done;
  print_newline ();
  for i = 0 to nrows - 1 do
    Printf.printf "%2d  " i;
    for j = 0 to ncols - 1 do
      Printf.printf "%s " (emoji_of_cell board.(i).(j))
    done;
    print_newline ()
  done;
  print_newline ()

(* ---------------------------------------------------------- *)
(*  Shared game data                                          *)
(* ---------------------------------------------------------- *)

let board_list : grid_state array array list =
  Cs3110_final_project.Initialize.board_list

let counter = ref 0
let client_output_channels : Lwt_io.output_channel list ref = ref []

(* let ship_sizes = [ 5; 4; 4; 3; 2; 2 ] *)
let ship_sizes = [ 1; 2; 3 ]
let client_usernames : string list ref = ref []
let ready_counter = ref 0
let client_ready_output_channels : Lwt_io.output_channel list ref = ref []
let current_player : Lwt_io.output_channel option ref = ref None

(* ---------------------------------------------------------- *)
(*  Error helpers                                             *)
(* ---------------------------------------------------------- *)

let fatal_error msg =
  prerr_endline msg;
  exit 1

let fatal_error_lwt msg =
  let%lwt () = Lwt_io.eprintl msg in
  exit 1

(* ---------------------------------------------------------- *)
(*  Networking utilities                                      *)
(* ---------------------------------------------------------- *)

let localhost ip port =
  try Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
  with Failure _ | Invalid_argument _ ->
    fatal_error ("Invalid IP address: " ^ ip)

let add_IP_and_port where =
  if Array.length Sys.argv < 4 then
    fatal_error ("Please enter the IP address and port for the " ^ where ^ ".");
  let ip = Sys.argv.(2) in
  let port =
    try int_of_string Sys.argv.(3)
    with Failure _ ->
      fatal_error "Invalid port number. Please provide an integer."
  in
  localhost ip port

let string_of_addr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let parse_client_username () =
  if Array.length Sys.argv <> 5 then fatal_error "Please enter a username.";
  if String.starts_with ~prefix:"\"" Sys.argv.(4) then
    String.sub Sys.argv.(4) 1 (String.length Sys.argv.(4) - 2)
  else Sys.argv.(4)

let notify_all message output_channel =
  let%lwt () = Lwt_io.fprintlf output_channel "%s" message in
  let%lwt () = Lwt_io.flush output_channel in
  Lwt.return ()

let verify_coord (str : string) : (int * int) option =
  let trimmed = String.trim str in
  let parts =
    trimmed |> String.split_on_char ' '
    |> List.filter (fun s -> String.length (String.trim s) > 0)
  in
  match parts with
  | [ a; b ] -> (
      match (int_of_string_opt a, int_of_string_opt b) with
      | Some x, Some y -> Some (x, y)
      | _ -> None)
  | _ -> None

(* ---------------------------------------------------------- *)
(*  SERVER: unchanged, still text-based                       *)
(* ---------------------------------------------------------- *)

let client_handler client_addr (client_in, client_out) : unit Lwt.t =
  let address_string = string_of_addr client_addr in
  let%lwt client_username = Lwt_io.read_line client_in in
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s (%s)." address_string
      client_username
  in
  let player_num =
    match !client_output_channels with
    | [] -> 0
    | [ _ ] -> 1
    | _ -> 1
  in
  client_usernames := client_username :: !client_usernames;
  client_output_channels := client_out :: !client_output_channels;

  (**************** WAITING ROOM ****************)
  let%lwt () = Lwt_io.fprintf client_out "You joined the game\n" in
  let%lwt () = Lwt_io.flush client_out in
  let%lwt () =
    Lwt_list.iter_p
      (notify_all (client_username ^ " joined the game"))
      !client_output_channels
  in
  incr counter;
  let%lwt () =
    let status = Printf.sprintf "Currently have %d/%d players" !counter 2 in
    let message =
      if !counter > 1 then status ^ "\nStarting game\n"
      else
        Printf.sprintf "%s\nWaiting for %d more player(s)..." status
          (2 - !counter)
    in
    Lwt_list.iter_p (notify_all message) !client_output_channels
  in
  let%lwt () = Lwt_io.flush client_out in

  (**************** GETTING MESSAGES FROM CLIENT ****************)
  let rec receive_message () =
    Lwt.catch
      (fun () ->
        let%lwt message = Lwt_io.read_line client_in in
        let msg = String.trim message in
        let%lwt () =
          Lwt_io.printlf "Message received from client %s (%s). %s"
            address_string client_username msg
        in
        let guess_handler msg r c sunk_coords =
          let others =
            List.filter (fun oc -> oc != client_out) !client_output_channels
          in
          let other_out =
            match others with
            | [ oc ] -> oc
            | _ -> client_out
          in
          match msg with
          | "Hit! Go again." ->
              let%lwt () =
                Lwt_io.write_line client_out
                  ("RESULT YOU HIT " ^ string_of_int r ^ " " ^ string_of_int c)
              in
              let%lwt () =
                Lwt_io.write_line other_out
                  ("RESULT OPPONENT HIT " ^ string_of_int r ^ " "
                 ^ string_of_int c)
              in
              let%lwt () = Lwt_io.write_line client_out "YOUR_TURN" in
              let%lwt () = Lwt_io.flush client_out in
              let%lwt () = Lwt_io.write_line other_out "OPPONENT_TURN" in
              let%lwt () =
                Lwt_io.write_line other_out
                  (client_username ^ " hit your ship!\n" ^ client_username
                 ^ " will go again.")
              in
              Lwt_io.flush other_out
          | "You sank a ship! Go again." ->
              let sunk_coords_str =
                String.concat " "
                  (List.flatten
                     (List.map
                        (fun (r, c) -> [ string_of_int r; string_of_int c ])
                        sunk_coords))
              in
              let%lwt () =
                Lwt_io.write_line client_out
                  ("RESULT YOU SINK " ^ sunk_coords_str)
              in
              let%lwt () =
                Lwt_io.write_line other_out
                  ("RESULT OPPONENT SINK " ^ string_of_int r ^ " "
                 ^ string_of_int c)
              in
              let%lwt () = Lwt_io.write_line client_out "YOUR_TURN" in
              let%lwt () = Lwt_io.flush client_out in
              let%lwt () =
                Lwt_io.write_line other_out
                  (client_username ^ " sunk your ship!\n" ^ client_username
                 ^ " will go again.")
              in
              Lwt_io.flush other_out
          | "Miss" ->
              let%lwt () =
                Lwt_io.write_line other_out
                  (client_username ^ " missed. Your turn.")
              in
              let%lwt () =
                Lwt_io.write_line other_out
                  ("RESULT OPPONENT MISS " ^ string_of_int r ^ " "
                 ^ string_of_int c)
              in
              let%lwt () = Lwt_io.write_line other_out "YOUR_TURN" in
              let%lwt () = Lwt_io.flush other_out in
              let%lwt () =
                Lwt_io.write_line client_out
                  ("RESULT YOU MISS " ^ string_of_int r ^ " " ^ string_of_int c)
              in
              let%lwt () = Lwt_io.write_line client_out "OPPONENT_TURN" in
              Lwt_io.flush client_out
          | msg when String.length msg >= 7 && String.sub msg 0 7 = "Player " ->
              Lwt_list.iter_p (notify_all msg) !client_output_channels
          | _ -> Lwt.return_unit
        in
        let handler : unit Lwt.t =
          match msg with
          | "BOARD_READY" ->
              incr ready_counter;
              let status =
                Printf.sprintf "%d/%d players finished setup" !ready_counter 2
              in
              client_ready_output_channels :=
                client_out :: !client_ready_output_channels;

              if !ready_counter >= 2 then begin
                Cs3110_final_project.Initialize.set_upd_lists ();
                begin
                  match !client_output_channels with
                  | p1 :: t ->
                      current_player := Some p1;
                      let status_msg =
                        status
                        ^ "\n\n\
                           Step 2 - Try to sink all the other player's ship \
                           first!\n"
                      in
                      let%lwt () =
                        Lwt_list.iter_p (notify_all status_msg)
                          !client_output_channels
                      in
                      let%lwt () = Lwt_io.write_line p1 "YOUR_TURN" in
                      let%lwt () = Lwt_io.flush p1 in
                      let%lwt () =
                        Lwt_list.iter_p
                          (fun p ->
                            if p == p1 then Lwt.return_unit
                            else
                              let%lwt () =
                                Lwt_io.write_line p "OPPONENT_TURN"
                              in
                              Lwt_io.flush p)
                          t
                      in
                      Lwt.return_unit
                  | _ -> Lwt.return_unit
                end
              end
              else
                let msg =
                  status ^ "\nWaiting for the other player to finish setup..."
                in
                Lwt_list.iter_p (notify_all msg) !client_ready_output_channels
          | string -> (
              match String.split_on_char ' ' string with
              | "PLACE" :: ship_idx_str :: coord_strs -> (
                  match int_of_string_opt ship_idx_str with
                  | None ->
                      Lwt_io.printl "Invalid PLACE: ship index is not an int"
                  | Some ship_idx -> (
                      let rec to_pairs acc = function
                        | r_str :: c_str :: rest -> (
                            match
                              (int_of_string_opt r_str, int_of_string_opt c_str)
                            with
                            | Some r, Some c -> to_pairs ((r, c) :: acc) rest
                            | _ -> Lwt.return_none)
                        | [] -> Lwt.return_some (List.rev acc)
                        | _ -> Lwt.return_none
                      in
                      let%lwt coords_opt = to_pairs [] coord_strs in
                      match coords_opt with
                      | None ->
                          Lwt_io.printl "Invalid PLACE: bad coordinate list"
                      | Some coords ->
                          let personal_idx = if player_num = 0 then 0 else 2 in
                          let personal_board =
                            List.nth board_list personal_idx
                          in
                          if player_num = 0 then
                            Cs3110_final_project.Initialize.place_ship
                              personal_board
                              (List.nth
                                 Cs3110_final_project.Initialize.ship_list0_og
                                 ship_idx)
                              coords
                          else
                            Cs3110_final_project.Initialize.place_ship
                              personal_board
                              (List.nth
                                 Cs3110_final_project.Initialize.ship_list1_og
                                 ship_idx)
                              coords;
                          Lwt.return_unit))
              | [ "GUESS"; r; c ] -> (
                  match (int_of_string_opt r, int_of_string_opt c) with
                  | Some row, Some col ->
                      let result_msg, _next_player, sunk_coords =
                        Cs3110_final_project.Turns.handle_turn (row, col)
                          player_num
                      in
                      let%lwt () = Lwt_io.write_line client_out result_msg in
                      guess_handler result_msg row col sunk_coords
                  | _ -> Lwt_io.printl "Invalid GUESS format: r or c not an int"
                  )
              | _ ->
                  Lwt_list.iter_p
                    (notify_all (client_username ^ " says: " ^ msg))
                    (List.filter
                       (fun output_channel -> output_channel != client_out)
                       !client_output_channels))
        in
        let%lwt () = handler in
        receive_message ())
      (function
        | End_of_file ->
            client_output_channels :=
              List.filter (( != ) client_out) !client_output_channels;
            let%lwt () =
              Lwt_list.iter_p
                (notify_all
                   (client_username ^ " left the game, ending the game."))
                !client_output_channels
            in
            let%lwt () =
              Lwt_list.iter_p (notify_all "END_GAME") !client_output_channels
            in
            Lwt_io.printlf "Client %s (%s) disconnected." address_string
              client_username
        | exn -> fatal_error ("Error: " ^ Printexc.to_string exn))
  in
  receive_message ()

let run_server () =
  let server () =
    let server_port = add_IP_and_port "server" in
    let%lwt () = Lwt_io.printlf "Server built successfully." in
    let%lwt _running_server =
      Lwt_io.establish_server_with_client_address server_port client_handler
    in
    let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
    never_resolved
  in
  Lwt_main.run (server ())

(* ---------------------------------------------------------- *)
(*  GTK CLIENT HELPERS                                       *)
(* ---------------------------------------------------------- *)

let coord_waiter : (int * int) Lwt.u option ref = ref None

let setup_coord_handlers (row_entry : GEdit.entry) (col_entry : GEdit.entry)
    (ok_button : GButton.button) =
  let try_submit () =
    match !coord_waiter with
    | None -> ()
    | Some w -> (
        let row_txt = row_entry#text in
        let col_txt = col_entry#text in
        match (int_of_string_opt row_txt, int_of_string_opt col_txt) with
        | Some r, Some c ->
            coord_waiter := None;
            row_entry#set_text "";
            col_entry#set_text "";
            Lwt.wakeup_later w (r, c)
        | _ -> ())
  in
  ignore (ok_button#connect#clicked ~callback:try_submit);
  ignore
    (col_entry#event#connect#key_press ~callback:(fun ev ->
         let key = GdkEvent.Key.keyval ev in
         if key = GdkKeysyms._Return || key = GdkKeysyms._KP_Enter then (
           try_submit ();
           true)
         else false))

let wait_for_coord () : (int * int) Lwt.t =
  let t, w = Lwt.wait () in
  coord_waiter := Some w;
  t

let make_board ~rows ~cols ~packing () =
  let table = GPack.table ~rows ~columns:cols ~homogeneous:true ~packing () in
  let buttons =
    Array.init rows (fun r ->
        Array.init cols (fun c ->
            let b = GButton.button ~label:" " () in
            table#attach ~left:c ~top:r ~expand:`BOTH ~fill:`BOTH b#coerce;
            b))
  in
  (table, buttons)

let render_board_gui (board : grid_state array array)
    (buttons : GButton.button array array) =
  let nrows = Array.length board in
  let ncols = Array.length board.(0) in
  for r = 0 to nrows - 1 do
    for c = 0 to ncols - 1 do
      buttons.(r).(c)#set_label (emoji_of_cell board.(r).(c))
    done
  done

let render_boards : (unit -> unit Lwt.t) ref = ref (fun () -> Lwt.return_unit)

(* ---------------------------------------------------------- *)
(*  CLIENT with GTK UI (including waiting room)              *)
(* ---------------------------------------------------------- *)

let client () =
  let window =
    GWindow.window ~title:"Battleship" ~border_width:10 ~width:900 ~height:600
      ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  let vbox = GPack.vbox ~spacing:10 ~packing:window#add () in
  let status_label =
    GMisc.label ~text:"Connecting to server..." ~packing:vbox#pack ()
  in

  let hbox = GPack.hbox ~spacing:20 ~packing:vbox#pack () in
  (* assume all boards same size as board_list.(0) *)
  let base_board = List.nth board_list 0 in
  let board_rows = Array.length base_board in
  let board_cols = Array.length base_board.(0) in

  let _left_table, left_buttons =
    make_board ~rows:board_rows ~cols:board_cols
      ~packing:(fun w -> hbox#pack ~expand:true ~fill:true w)
      ()
  in
  let _right_table, right_buttons =
    make_board ~rows:board_rows ~cols:board_cols
      ~packing:(fun w -> hbox#pack ~expand:true ~fill:true w)
      ()
  in

  let coord_box = GPack.hbox ~spacing:5 ~packing:vbox#pack () in
  let row_entry =
    GEdit.entry ~placeholder_text:"Row" ~packing:coord_box#pack ()
  in
  let col_entry =
    GEdit.entry ~placeholder_text:"Col" ~packing:coord_box#pack ()
  in
  let ok_button = GButton.button ~label:"OK" ~packing:coord_box#pack () in
  setup_coord_handlers row_entry col_entry ok_button;

  window#show ();

  let game_started_waiter, wake_game_started = Lwt.wait () in
  let board_finished_waiter, wake_board_finished = Lwt.wait () in
  let connect_port = add_IP_and_port "client" in
  let client_username = parse_client_username () in
  client_usernames := client_username :: !client_usernames;

  let%lwt server_in, server_out = Lwt_io.open_connection connect_port in
  let%lwt () = Lwt_io.write_line server_out client_username in
  let%lwt () = Lwt_io.flush server_out in

  let player_num =
    if List.nth !client_usernames 0 = client_username then 0 else 1
  in
  let personal_idx = if player_num = 0 then 0 else 2 in
  let attack_idx = if player_num = 0 then 1 else 3 in

  (render_boards :=
     fun () ->
       render_board_gui (List.nth board_list personal_idx) left_buttons;
       render_board_gui (List.nth board_list attack_idx) right_buttons;
       Lwt.return_unit);

  (* track whether we've left the waiting room *)
  let game_started = ref false in

  (* ------------ SHIP SETUP USING GUI COORD INPUT ------------ *)
  let rec init_game count =
    let personal_board = List.nth board_list personal_idx in
    let ship_size = List.nth ship_sizes count in

    let rec read_coordinates i max ship_lst =
      if i > max then Lwt.return ship_lst
      else
        let%lwt () =
          Lwt.return
            (status_label#set_text
               (Printf.sprintf
                  "Place ship %d (size %d): add coordinate %d of %d (row col)"
                  (count + 1) ship_size i max))
        in
        let%lwt r, c = wait_for_coord () in
        if r < 0 || c < 0 || r >= board_rows || c >= board_cols then
          let%lwt () =
            Lwt.return
              (status_label#set_text
                 "Invalid coordinate: out of bounds. Try again.")
          in
          read_coordinates i max ship_lst
        else read_coordinates (i + 1) max ((r, c) :: ship_lst)
    in

    let rec place_this_ship () =
      let%lwt ship_coords = read_coordinates 1 ship_size [] in
      let ship_coords = List.rev ship_coords in
      Lwt.catch
        (fun () ->
          if player_num = 0 then
            Cs3110_final_project.Initialize.place_ship personal_board
              (List.nth Cs3110_final_project.Initialize.ship_list0_og count)
              ship_coords
          else
            Cs3110_final_project.Initialize.place_ship personal_board
              (List.nth Cs3110_final_project.Initialize.ship_list1_og count)
              ship_coords;

          let%lwt () = !render_boards () in

          let coord_strings =
            List.flatten
              (List.map
                 (fun (r, c) -> [ string_of_int r; string_of_int c ])
                 ship_coords)
          in
          let place_message =
            "PLACE " ^ string_of_int count ^ " "
            ^ String.concat " " coord_strings
          in
          let%lwt () = Lwt_io.write_line server_out place_message in
          let%lwt () = Lwt_io.flush server_out in
          Lwt.return_unit)
        (function
          | Failure msg ->
              let%lwt () =
                Lwt.return (status_label#set_text ("Invalid placement: " ^ msg))
              in
              place_this_ship ()
          | exn -> Lwt.fail exn)
    in

    let%lwt () = place_this_ship () in
    if count + 1 < List.length ship_sizes then init_game (count + 1)
    else begin
      let%lwt () = !render_boards () in
      let%lwt () = Lwt_io.write_line server_out "BOARD_READY" in
      let%lwt () = Lwt_io.flush server_out in
      Lwt.return_unit
    end
  in

  (* ------------ TURN TAKING USING GUI COORD INPUT ------------ *)
  let rec guess () =
    let%lwt () =
      Lwt.return
        (status_label#set_text "Your turn! Enter guess (row col) and press OK.")
    in
    let%lwt r, c = wait_for_coord () in
    if r < 0 || c < 0 || r >= board_rows || c >= board_cols then
      let%lwt () =
        Lwt.return
          (status_label#set_text
             "Invalid coordinate for guess. Please try again.")
      in
      guess ()
    else
      let%lwt () =
        Lwt_io.write_line server_out (Printf.sprintf "GUESS %d %d" r c)
      in
      Lwt_io.flush server_out
  in

  (* ------------ CONSTANTLY CHECK MESSAGES FROM SERVER ---------- *)
  let rec check_server () =
    let%lwt message_opt = Lwt_io.read_line_opt server_in in
    match message_opt with
    | None -> fatal_error_lwt "\nServer disconnected."
    | Some msg ->
        let msg = String.trim msg in
        (* still log to terminal for debugging *)
        let%lwt () = Lwt_io.printlf "%s" msg in
        let handler =
          match msg with
          | "Starting game" ->
              game_started := true;
              let%lwt () =
                Lwt.return
                  (status_label#set_text
                     "Game starting! Step 1 – set up your ships.")
              in
              Lwt.return (Lwt.wakeup_later wake_game_started ())
          | "BOARD_READY" ->
              (* server uses this internally; client just wakes promise *)
              Lwt.return (Lwt.wakeup_later wake_board_finished ())
          | "YOUR_TURN" ->
              let%lwt () = !render_boards () in
              let%lwt () =
                Lwt.return
                  (status_label#set_text "It's your turn! Enter a guess.")
              in
              let%lwt () = guess () in
              Lwt.return_unit
          | "OPPONENT_TURN" ->
              let%lwt () =
                Lwt.return
                  (status_label#set_text "Opponent's turn. Please wait...")
              in
              Lwt.return_unit
          | "END_GAME" ->
              let%lwt () =
                Lwt.return
                  (status_label#set_text "Game over. Closing client...")
              in
              fatal_error "\nExiting game..."
          | m when String.starts_with ~prefix:"You joined the game" m ->
              let%lwt () = Lwt.return (status_label#set_text m) in
              Lwt.return_unit
          | m
            when String.ends_with ~suffix:"joined the game" m
                 || String.starts_with ~prefix:"Currently have" m
                 || String.starts_with ~prefix:"Waiting for" m ->
              (* Waiting room messages from server *)
              let%lwt () = Lwt.return (status_label#set_text m) in
              Lwt.return_unit
          | m when String.starts_with ~prefix:"RESULT " m ->
              let parts = String.split_on_char ' ' m in
              let attack_board = List.nth board_list attack_idx in
              let personal_board = List.nth board_list personal_idx in
              let sink_ship_at_coord r c ship_list_og board =
                match
                  List.find_opt
                    (fun ship ->
                      Cs3110_final_project.Initialize.CoordSet.mem (r, c)
                        ship.coords)
                    ship_list_og
                with
                | None ->
                    Printf.printf
                      "WARNING: sink_ship_at_coord could not find a ship at \
                       (%d,%d)\n"
                      r c
                | Some ship ->
                    Cs3110_final_project.Initialize.CoordSet.iter
                      (fun (sr, sc) ->
                        board.(sr).(sc) <- Cs3110_final_project.Initialize.SINK)
                      ship.coords
              in
              let update =
                match parts with
                | [ "RESULT"; "YOU"; "HIT"; r; c ] ->
                    let r = int_of_string r in
                    let c = int_of_string c in
                    attack_board.(r).(c) <- HIT;
                    Lwt.return_unit
                | "RESULT" :: "YOU" :: "SINK" :: sunk_coords_str ->
                    let rec lst_of_str acc = function
                      | r :: c :: t ->
                          lst_of_str
                            ((int_of_string r, int_of_string c) :: acc)
                            t
                      | [] -> List.rev acc
                      | _ -> acc
                    in
                    let sunk_coords = lst_of_str [] sunk_coords_str in
                    List.iter
                      (fun (r, c) -> attack_board.(r).(c) <- SINK)
                      sunk_coords;
                    Lwt.return_unit
                | [ "RESULT"; "YOU"; "MISS"; r; c ] ->
                    let r = int_of_string r in
                    let c = int_of_string c in
                    attack_board.(r).(c) <- MISS;
                    Lwt.return_unit
                | [ "RESULT"; "OPPONENT"; "HIT"; r; c ] ->
                    let r = int_of_string r in
                    let c = int_of_string c in
                    personal_board.(r).(c) <- HIT;
                    Lwt.return_unit
                | [ "RESULT"; "OPPONENT"; "SINK"; r; c ] ->
                    let r = int_of_string r in
                    let c = int_of_string c in
                    let your_og =
                      if List.nth !client_usernames 0 = client_username then
                        Cs3110_final_project.Initialize.ship_list0_og
                      else Cs3110_final_project.Initialize.ship_list1_og
                    in
                    sink_ship_at_coord r c your_og personal_board;
                    Lwt.return_unit
                | [ "RESULT"; "OPPONENT"; "MISS"; r; c ] ->
                    let r = int_of_string r in
                    let c = int_of_string c in
                    personal_board.(r).(c) <- MISS;
                    Lwt.return_unit
                | _ -> Lwt.return_unit
              in
              let%lwt () = update in
              let%lwt () = !render_boards () in
              Lwt.return_unit
          | _ ->
              (* Any other message: during waiting room, show it in status. *)
              let%lwt () =
                if not !game_started then Lwt.return (status_label#set_text msg)
                else Lwt.return ()
              in
              Lwt.return_unit
        in
        let%lwt () = handler in
        check_server ()
  in

  (* ------------ PROMISES ORDERING: WAITING ROOM, SETUP, GAME -------- *)
  let check_p = check_server () in
  let%lwt () = game_started_waiter in
  let%lwt () =
    Lwt.return (status_label#set_text "Step 1 – set up your ships!")
  in
  let%lwt () = init_game 0 in
  let%lwt () = board_finished_waiter in
  let%lwt () =
    Lwt.return
      (status_label#set_text "All players finished setup. Waiting for turns...")
  in
  check_p

let run_client () =
  let _ = GMain.init () in
  Lwt_glib.install ();
  Lwt.async client;
  GMain.main ()

(* ---------------------------------------------------------- *)
(*  Entry point                                               *)
(* ---------------------------------------------------------- *)

let _ =
  let print_usage () =
    Printf.printf "Please specify whether you are a server or client"
  in
  if Array.length Sys.argv < 1 then print_usage ()
  else
    match Sys.argv.(1) with
    | "server" -> run_server ()
    | "client" -> run_client ()
    | _ -> print_usage ()
