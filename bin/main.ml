open Cs3110_final_project.Initialize

(**************** UI PRINTS ****************)
let emoji_of_cell = function
  | EMPTY -> "🌊"
  | SHIP -> "🚢"
  | HIT -> "💣"
  | MISS -> "❌"
  | SINK -> "☠️ "

let print_two_boards board_left board_right : unit Lwt.t =
  let nrows = Array.length board_left in
  let ncols = Array.length board_left.(0) in

  let%lwt () = Lwt_io.print "      " in

  (* Column headers for left board *)
  let rec print_cols j =
    if j = ncols then Lwt.return_unit
    else
      let%lwt () = Lwt_io.printf "%d  " j in
      print_cols (j + 1)
  in
  let%lwt () = print_cols 0 in

  (* gap between boards *)
  let%lwt () = Lwt_io.print "      " in

  (* Column headers for right board *)
  let%lwt () = print_cols 0 in
  let%lwt () = Lwt_io.print "\n" in

  (* Print each row *)
  let rec print_rows i =
    if i = nrows then Lwt.return_unit
    else
      let%lwt () = Lwt_io.printf "%2d   " i in

      (* left board cells *)
      let rec print_left j =
        if j = ncols then Lwt.return_unit
        else
          let%lwt () = Lwt_io.print (emoji_of_cell board_left.(i).(j) ^ " ") in
          print_left (j + 1)
      in
      let%lwt () = print_left 0 in

      let%lwt () = Lwt_io.print "   |   " in

      (* right board cells *)
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

  (* print column headers *)
  Printf.printf "    ";
  for j = 0 to ncols - 1 do
    Printf.printf "%d  " j
  done;
  print_newline ();

  (* print each row with a row label *)
  for i = 0 to nrows - 1 do
    Printf.printf "%2d  " i;
    for j = 0 to ncols - 1 do
      Printf.printf "%s " (emoji_of_cell board.(i).(j))
    done;
    print_newline ()
  done;
  print_newline ()

let title = "\027[36m"
let steps = "\027[35m"
let win = "\027[32m"
let lose = "\027[31m"
let reset = "\027[0m"

let battleship_banner =
  title ^ "                                 /|\\\n"
  ^ "                                /_|_\\\n"
  ^ "        W E L C O M E   T O      _|___\n"
  ^ "        B A T T L E S H I P      \\___/\n"
  ^ "                                 ~~~~~\n" ^ reset

let you_win = win ^ "\n\n    YOU WIN\t\tദ്ദി(˵ • ᴗ • ˵ )\n\n"
let you_lose = lose ^ "\n\n     YOU LOSE\t\t.·°՞(っ-ᯅ-ς)՞°·.\n\n"

(**************** GLOBAL VARIABLES ****************)
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

(** [fatal_error msg] will print an error message [msg] and then end the
    execution *)
let fatal_error msg =
  prerr_endline msg;
  exit 1

(** [fatal_error_lwt msg] will print an error message [msg] with Lwt and then
    end the execution *)
let fatal_error_lwt msg =
  let%lwt () = Lwt_io.eprintl msg in
  exit 1

let localhost ip port =
  try Unix.ADDR_INET (Unix.inet_addr_of_string ip, port)
  with Failure _ | Invalid_argument _ ->
    fatal_error ("Invalid IP address: " ^ ip)

(** [add_IP_and_port where] builds a Unix address for [where]*)
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

(** [string_of_addr addr] converts a Unix address into a string *)
let string_of_addr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

(** [parse_client_username] will parse the username argument of a client *)
let parse_client_username () =
  if Array.length Sys.argv <> 5 then fatal_error "Please enter a username.";
  if String.starts_with ~prefix:"\"" Sys.argv.(4) then
    String.sub Sys.argv.(4) 1 (String.length Sys.argv.(4) - 2)
  else Sys.argv.(4)

(** [notify_all message output_channel] will notify all the clients [message]*)
let notify_all message output_channel =
  let%lwt () = Lwt_io.fprintlf output_channel "%s" message in
  let%lwt () = Lwt_io.flush output_channel in
  Lwt.return ()

(** [verify_coord str] will verify if the [str] are 2 integers *)
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

(** [reset_game ()] resets all the game variables *)
let reset_game () =
  client_output_channels := [];
  client_usernames := [];
  counter := 0;
  ready_counter := 0;
  client_ready_output_channels := [];
  current_player := None

let client_handler client_addr (client_in, client_out) : unit Lwt.t =
  (* connecting to the client *)
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
      (List.filter (fun oc -> oc != client_out) !client_output_channels)
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
                  (client_username ^ " hit your ship! " ^ client_username
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
          | "Enter a coordinate that has not already been entered" ->
              let%lwt () = Lwt_io.write_line client_out "YOUR_TURN" in
              Lwt_io.flush client_out
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
              let%lwt () = Lwt_io.write_line client_out "YOU WIN" in
              let%lwt () = Lwt_io.flush client_out in
              Lwt_list.iter_p (notify_all "YOU LOSE")
                (List.filter
                   (fun output_channel -> output_channel != client_out)
                   !client_output_channels)
          | _ -> Lwt.return_unit
        in
        (* Each branch returns unit Lwt.t *)
        let handler : unit Lwt.t =
          match msg with
          (**************** PLAYER FINISHED SETTING SHIPS ****************)
          | "BOARD_READY" ->
              incr ready_counter;
              let status =
                Printf.sprintf "%d/%d players finished setup" !ready_counter 2
              in
              (* Track which clients have sent BOARD_READY *)
              client_ready_output_channels :=
                client_out :: !client_ready_output_channels;

              if !ready_counter >= 2 then begin
                Cs3110_final_project.Initialize.set_upd_lists ();
                (* Both players are ready. Chooses first client as starting
                   player. *)
                match !client_output_channels with
                | p1 :: t ->
                    current_player := Some p1;
                    let status_msg =
                      status ^ steps
                      ^ "\n\n\
                         Step 2 ~ Try to sink all the other player's ship \
                         first!" ^ reset
                    in
                    let%lwt () =
                      Lwt_list.iter_p (notify_all status_msg)
                        !client_output_channels
                    in
                    (* Notifying p1 it's their turn *)
                    let%lwt () = Lwt_io.write_line p1 "YOUR_TURN" in
                    let%lwt () = Lwt_io.flush p1 in
                    (* Notifying everyone it's opponent's turn *)
                    let%lwt () =
                      Lwt_list.iter_p
                        (fun p ->
                          if p == p1 then Lwt.return_unit
                          else
                            let%lwt () = Lwt_io.write_line p "OPPONENT_TURN" in
                            Lwt_io.flush p)
                        t
                    in
                    Lwt.return_unit
                | _ -> Lwt.return_unit
              end
              else begin
                (* Not all players finished setting up boards *)
                let msg =
                  status ^ "\nWaiting for the other player to finish setup..."
                in
                Lwt_list.iter_p (notify_all msg) !client_ready_output_channels
              end
          | string -> (
              match String.split_on_char ' ' string with
              | "PLACE" :: ship_idx_str :: coord_strs -> (
                  match int_of_string_opt ship_idx_str with
                  | None ->
                      Lwt_io.printl "Invalid PLACE: ship index is not an int"
                  | Some ship_idx -> (
                      (* parse coords as (r,c) pairs *)
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
                          (* print_board (List.nth board_list personal_idx); *)
                          Lwt.return_unit))
              | [ "GUESS"; r; c ] -> (
                  match (int_of_string_opt r, int_of_string_opt c) with
                  | Some row, Some col ->
                      let result_msg, next_player, sunk_coords =
                        Cs3110_final_project.Turns.handle_turn (row, col)
                          player_num
                      in
                      let%lwt () = Lwt_io.write_line client_out result_msg in
                      guess_handler result_msg row col sunk_coords
                  | _ -> Lwt_io.printl "Invalid GUESS format: r or c not an int"
                  )
              | _ ->
                  (* Existing chat logic *)
                  Lwt_list.iter_p
                    (notify_all (client_username ^ " says: " ^ msg))
                    (List.filter
                       (fun oc -> oc != client_out)
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
            reset_game ();
            Lwt_io.printlf "Client %s (%s) disconnected." address_string
              client_username
        | exn -> fatal_error ("Error: " ^ Printexc.to_string exn))
  in
  receive_message ()

let run_server () =
  let server () =
    let server_port = add_IP_and_port "server" in
    let%lwt () = Lwt_io.printlf "Server built successfully." in
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address server_port client_handler
    in
    let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
    never_resolved
  in
  Lwt_main.run (server ())

let run_client () =
  let client () =
    Lwt.catch
      (fun () ->
        (* will be resolved when enough people joined the game *)
        let game_started_waiter, wake_game_started = Lwt.wait () in
        let board_finished_waiter, wake_board_finished = Lwt.wait () in
        let connect_port = add_IP_and_port "client" in
        let client_username = parse_client_username () in
        client_usernames := client_username :: !client_usernames;
        let%lwt () = Lwt_io.printlf "%s" battleship_banner in
        let%lwt server_in, server_out = Lwt_io.open_connection connect_port in
        let%lwt () = Lwt_io.write_line server_out client_username in
        let%lwt () = Lwt_io.flush server_out in

        (**************** INITIALIZE GAME FUNCTION ****************)
        let player_num =
          if List.nth !client_usernames 0 = client_username then 0 else 1
        in
        let rec init_game count =
          let personal_idx = if player_num = 0 then 0 else 2 in
          let%lwt () =
            if count <> 0 then print_board (List.nth board_list personal_idx);
            Lwt_io.print ("Add ship " ^ string_of_int (count + 1) ^ "\n")
          in
          let ship_size = List.nth ship_sizes count in

          (* Read all ship coordinates from user *)
          let rec read_coordinates i max ship_lst =
            if i > max then Lwt.return ship_lst
            else
              let%lwt () =
                Lwt_io.print ("Add coordinate " ^ string_of_int i ^ ": ")
              in
              let%lwt coord_opt = Lwt_io.read_line_opt Lwt_io.stdin in
              match coord_opt with
              | None -> fatal_error_lwt "Input closed. Exiting client."
              | Some message -> (
                  match verify_coord message with
                  | None ->
                      let%lwt () =
                        Lwt_io.printl
                          "Invalid coordinate. Please enter two integers like: \
                           1 2"
                      in
                      read_coordinates i max ship_lst
                  | Some coord ->
                      read_coordinates (i + 1) max (coord :: ship_lst))
          in

          (* Helper: place THIS ship (count) with retry on failure *)
          let rec place_this_ship () =
            let%lwt ship_coords = read_coordinates 1 ship_size [] in
            let ship_coords = List.rev ship_coords in
            let personal_board = List.nth board_list personal_idx in

            Lwt.catch
              (fun () ->
                (* try placing locally *)
                if player_num = 0 then
                  Cs3110_final_project.Initialize.place_ship personal_board
                    (List.nth Cs3110_final_project.Initialize.ship_list0_og
                       count)
                    ship_coords
                else
                  Cs3110_final_project.Initialize.place_ship personal_board
                    (List.nth Cs3110_final_project.Initialize.ship_list1_og
                       count)
                    ship_coords;

                (* if we got here, placement succeeded; now mirror to server *)
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
                    (* placement invalid: tell user and RETRY this same ship *)
                    let%lwt () = Lwt_io.printl msg in
                    place_this_ship ()
                | exn -> Lwt.fail exn)
          in

          (* ensure this ship is successfully placed before moving on *)
          let%lwt () = place_this_ship () in

          (* move to next ship or finish *)
          if count + 1 < List.length ship_sizes then init_game (count + 1)
          else begin
            let personal_board = List.nth board_list personal_idx in
            print_board personal_board;
            let%lwt () = Lwt_io.write_line server_out "BOARD_READY" in
            let%lwt () = Lwt_io.flush server_out in
            Lwt.return_unit
          end
        in

        (**************** TURN TAKING FUNCTION ****************)
        let rec guess () =
          let%lwt () = Lwt_io.print "Your turn! Enter guess (row col): " in
          let stdin_read = Lwt_io.read_line_opt Lwt_io.stdin in
          let server_read =
            Lwt.catch
              (fun () ->
                let%lwt _ = Lwt_io.read_line_opt server_in in
                Lwt.return `ServerData)
              (function
                | End_of_file -> Lwt.fail End_of_file
                | exn -> Lwt.fail exn)
          in

          let%lwt coord_opt =
            Lwt.pick
              [
                (let%lwt input = stdin_read in
                 Lwt.return input);
                (let%lwt _ = server_read in
                 fatal_error_lwt "\nServer disconnected.");
              ]
          in
          (* let%lwt coord_opt = Lwt_io.read_line_opt Lwt_io.stdin in *)
          match coord_opt with
          | None ->
              (* Assuming fatal_error : string -> 'a Lwt.t *)
              fatal_error_lwt "Input closed. Exiting client."
          | Some message -> (
              match verify_coord message with
              | Some (r, c) ->
                  let%lwt () =
                    Lwt_io.write_line server_out
                      (Printf.sprintf "GUESS %d %d" r c)
                  in
                  Lwt_io.flush server_out
              | None ->
                  let%lwt () =
                    Lwt_io.printl
                      "Invalid coordinate. Please enter two integers like: 1 2"
                  in
                  guess ())
        in

        (**************** CHECKING MESSAGES FROM SERVER ****************)
        let rec check_server () =
          let%lwt message_opt = Lwt_io.read_line_opt server_in in
          match message_opt with
          | None -> fatal_error_lwt "\nServer disconnected."
          | Some msg ->
              let msg = String.trim msg in
              let handler =
                match msg with
                | "Starting game" ->
                    Lwt.return (Lwt.wakeup_later wake_game_started ())
                | "BOARD_READY" ->
                    Lwt.return (Lwt.wakeup_later wake_board_finished ())
                | "YOUR_TURN" ->
                    let player_num =
                      if List.nth !client_usernames 0 = client_username then 0
                      else 1
                    in
                    let%lwt () =
                      if player_num = 0 then
                        print_two_boards (List.nth board_list 0)
                          (List.nth board_list 1)
                      else
                        print_two_boards (List.nth board_list 2)
                          (List.nth board_list 3)
                    in
                    let%lwt () = guess () in
                    Lwt.return_unit
                | "END_GAME" ->
                    reset_game ();
                    fatal_error "\nExiting game..."
                | m when String.starts_with ~prefix:"RESULT " m ->
                    let parts = String.split_on_char ' ' m in
                    begin
                      let attack_idx =
                        if List.nth !client_usernames 0 = client_username then 1
                        else 3
                      in
                      let attack_board = List.nth board_list attack_idx in
                      let personal_idx =
                        if List.nth !client_usernames 0 = client_username then 0
                        else 2
                      in
                      let personal_board = List.nth board_list personal_idx in
                      let sink_ship_at_coord r c ship_list_og board =
                        (* Find the ship whose original coords contain this
                           point *)
                        match
                          List.find_opt
                            (fun ship ->
                              Cs3110_final_project.Initialize.CoordSet.mem
                                (r, c) ship.coords)
                            ship_list_og
                        with
                        | None ->
                            Printf.printf
                              "WARNING: sink_ship_at_coord could not find a \
                               ship at (%d,%d)\n"
                              r c
                        | Some ship ->
                            Cs3110_final_project.Initialize.CoordSet.iter
                              (fun (sr, sc) ->
                                board.(sr).(sc) <-
                                  Cs3110_final_project.Initialize.SINK)
                              ship.coords
                      in
                      match parts with
                      | [ "RESULT"; "YOU"; "HIT"; r; c ] ->
                          let r = int_of_string r in
                          let c = int_of_string c in
                          attack_board.(r).(c) <- HIT;
                          Lwt.return ()
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
                          Lwt.return ()
                      | [ "RESULT"; "YOU"; "MISS"; r; c ] ->
                          let r = int_of_string r in
                          let c = int_of_string c in
                          attack_board.(r).(c) <- MISS;
                          Lwt.return ()
                      | [ "RESULT"; "OPPONENT"; "HIT"; r; c ] ->
                          let r = int_of_string r in
                          let c = int_of_string c in
                          personal_board.(r).(c) <- HIT;
                          Lwt.return ()
                      | [ "RESULT"; "OPPONENT"; "SINK"; r; c ] ->
                          let r = int_of_string r in
                          let c = int_of_string c in
                          let your_og =
                            if List.nth !client_usernames 0 = client_username
                            then Cs3110_final_project.Initialize.ship_list0_og
                            else Cs3110_final_project.Initialize.ship_list1_og
                          in
                          sink_ship_at_coord r c your_og personal_board;
                          Lwt.return ()
                      | [ "RESULT"; "OPPONENT"; "MISS"; r; c ] ->
                          let r = int_of_string r in
                          let c = int_of_string c in
                          personal_board.(r).(c) <- MISS;
                          Lwt.return ()
                      | _ -> Lwt.return ()
                    end
                | "OPPONENT_TURN" ->
                    let%lwt () =
                      Lwt_io.printl "Waiting for other player to go..."
                    in
                    Lwt.return_unit
                | "YOU WIN" ->
                    let%lwt () = Lwt_io.printlf "%s%s" you_lose reset in
                    Lwt.return_unit
                | "YOU LOSE" ->
                    let%lwt () = Lwt_io.printlf "%s%s" you_lose reset in
                    Lwt.return_unit
                | msg ->
                    (* Default: only print messages that are NOT protocol
                       commands *)
                    let no_print_messages =
                      [
                        "BOARD_READY";
                        "PLACE";
                        "GUESS";
                        "RESULT";
                        "Player 0 wins!";
                        "Player 1 wins!";
                      ]
                    in
                    let first_word =
                      match String.split_on_char ' ' msg with
                      | cmd :: _ -> cmd
                      | [] -> msg
                    in
                    if List.mem first_word no_print_messages then
                      (* Suppress these low-level protocol messages *)
                      Lwt.return_unit
                    else
                      let%lwt () = Lwt_io.printlf "%s" msg in
                      Lwt.return_unit
              in
              let%lwt () = handler in
              check_server ()
        in

        (**************** CREATING ALL PROMISES ****************)
        let check_p = check_server () in
        let%lwt () = game_started_waiter in
        let%lwt () =
          Lwt_io.printl (steps ^ "Step 1 ~ place your ships!" ^ reset)
        in
        let%lwt () = init_game 0 in
        let%lwt () = board_finished_waiter in
        check_p)
      (function
        | End_of_file | Unix.Unix_error _ ->
            let%lwt () = Lwt_io.printl "\nServer disconnected. Exiting..." in
            reset_game ();
            exit 0
        | exn -> Lwt.fail exn)
  in
  Lwt_main.run (client ())

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
