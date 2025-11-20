open Gtk
open Cs3110_final_project.Initialize

let emoji_of_cell = function
  | EMPTY -> "🌊"
  | SHIP -> "🚢"
  | HIT -> "💥"
  | MISS -> "❌"
  | SINK -> "☠️"

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

let client_handler client_addr (client_in, client_out) : unit Lwt.t =
  (* connecting to the client *)
  let address_string = string_of_addr client_addr in
  let%lwt client_username = Lwt_io.read_line client_in in
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s (%s)." address_string
      client_username
  in

  (* notifying everyone of the connection *)
  let%lwt () = Lwt_io.fprintf client_out "You joined the game\n" in
  let%lwt () = Lwt_io.flush client_out in
  let%lwt () =
    Lwt_list.iter_p
      (notify_all (client_username ^ " joined the game"))
      !client_output_channels
  in
  client_output_channels := client_out :: !client_output_channels;
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

  (* turn taking *)
  let rec receive_message () =
    Lwt.catch
      (fun () ->
        let%lwt message = Lwt_io.read_line client_in in
        let msg = String.trim message in
        let%lwt () =
          Lwt_io.printlf "Message received from client %s (%s). %s"
            address_string client_username msg
        in

        (* Each branch returns unit Lwt.t *)
        let handler : unit Lwt.t =
          match msg with
          | "BOARD_READY" ->
              incr ready_counter;
              let status =
                Printf.sprintf "%d/%d players finished setup" !ready_counter 2
              in

              (* Track which clients have sent BOARD_READY, if you still need
                 this *)
              client_ready_output_channels :=
                client_out :: !client_ready_output_channels;

              if !ready_counter >= 2 then begin
                (* Both players are ready. Choose a starting player. *)
                match !client_output_channels with
                | p1 :: t ->
                    current_player := Some p1;
                    let status_msg =
                      status
                      ^ "\n\n\
                         Step 2 - Try to sink all the other player's ship first!\n"
                    in
                    let%lwt () =
                      Lwt_list.iter_p (notify_all status_msg)
                        !client_output_channels
                    in
                    (* p1's turn *)
                    let%lwt () = Lwt_io.write_line p1 "YOUR_TURN" in
                    let%lwt () = Lwt_io.flush p1 in
                    (* everyone else: opponent's turn *)
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
                | _ ->
                    (* Shouldn't happen with 2-player game *)
                    Lwt.return_unit
              end
              else begin
                (* Only one player ready so far *)
                client_ready_output_channels :=
                  client_out :: !client_ready_output_channels;
                let msg =
                  status ^ "\nWaiting for the other player to finish setup..."
                in
                Lwt_list.iter_p (notify_all msg) !client_ready_output_channels
              end
          | _ ->
              (* Existing chat logic *)
              Lwt_list.iter_p
                (notify_all (client_username ^ " says: " ^ msg))
                (List.filter
                   (fun output_channel -> output_channel != client_out)
                   !client_output_channels)
        in
        let%lwt () = handler in
        receive_message ())
      (function
        | End_of_file ->
            client_output_channels :=
              List.filter (( != ) client_out) !client_output_channels;
            let%lwt () =
              Lwt_list.iter_p
                (notify_all (client_username ^ " left the chat"))
                !client_output_channels
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
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address server_port client_handler
    in
    let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
    never_resolved
  in
  Lwt_main.run (server ())

let run_client () =
  let client () =
    (* establishing usernames and connecting to server *)
    let game_started_waiter, wake_game_started = Lwt.wait () in
    let board_finished_waiter, wake_board_finished = Lwt.wait () in
    let connect_port = add_IP_and_port "client" in
    let client_username = parse_client_username () in
    client_usernames := client_username :: !client_usernames;
    let%lwt server_in, server_out = Lwt_io.open_connection connect_port in
    let%lwt () = Lwt_io.write_line server_out client_username in
    let%lwt () = Lwt_io.flush server_out in

    let rec check_server () =
      let%lwt message_opt = Lwt_io.read_line_opt server_in in
      match message_opt with
      | None -> fatal_error "\nServer disconnected."
      | Some msg ->
          let msg = String.trim msg in
          let%lwt () = Lwt_io.printlf "%s" msg in
          (* This match MUST return unit Lwt.t in every branch *)
          let handler =
            match msg with
            | "Starting game" ->
                (* wakeup_later is just unit, so wrap it in Lwt.return *)
                Lwt.return (Lwt.wakeup_later wake_game_started ())
            | "BOARD_READY" ->
                Lwt.return (Lwt.wakeup_later wake_board_finished ())
            | "YOUR_TURN" -> (
                let%lwt () =
                  Lwt_io.print "Your turn! Enter guess (row col): "
                in
                let%lwt coord_opt = Lwt_io.read_line_opt Lwt_io.stdin in
                match coord_opt with
                | None -> fatal_error "Input closed. Exiting client."
                | Some coord_line ->
                    let%lwt () =
                      Lwt_io.write_line server_out ("GUESS " ^ coord_line)
                    in
                    Lwt_io.flush server_out)
            | _ -> Lwt.return_unit
          in
          let%lwt () = handler in
          check_server ()
    in
    let check_p = check_server () in
    let%lwt () = game_started_waiter in

    (* initialize game *)
    let player_num =
      if List.nth !client_usernames 0 = client_username then 0 else 1
    in
    let rec init_game count =
      let%lwt () =
        if count <> 0 then print_board (List.nth board_list player_num);
        Lwt_io.print ("Add ship " ^ string_of_int (count + 1) ^ "\n")
      in
      let ship_size = List.nth ship_sizes count in
      let rec read_coordinates i max ship_lst =
        if i > max then Lwt.return ship_lst
        else
          let%lwt () =
            Lwt_io.print ("Add coordinate " ^ string_of_int i ^ ": ")
          in
          let%lwt coord_opt = Lwt_io.read_line_opt Lwt_io.stdin in
          match coord_opt with
          | None -> fatal_error "Input closed. Exiting client."
          | Some message -> (
              match verify_coord message with
              | None ->
                  let%lwt () =
                    Lwt_io.printl
                      "Invalid coordinate. Please enter two integers like: 1 2"
                  in
                  read_coordinates i max ship_lst
              | Some coord -> read_coordinates (i + 1) max (coord :: ship_lst))
      in
      let%lwt ship_coords = read_coordinates 1 ship_size [] in
      if player_num = 0 then begin
        Cs3110_final_project.Initialize.place_ship (List.nth board_list 0)
          (List.nth Cs3110_final_project.Initialize.ship_list0_og count)
          ship_coords
      end
      else begin
        Cs3110_final_project.Initialize.place_ship (List.nth board_list 1)
          (List.nth Cs3110_final_project.Initialize.ship_list1_og count)
          ship_coords
      end;
      if count + 1 < List.length ship_sizes then init_game (count + 1)
      else begin
        print_board (List.nth board_list player_num);
        let%lwt () = Lwt_io.write_line server_out "BOARD_READY" in
        let%lwt () = Lwt_io.flush server_out in
        Lwt.return_unit
      end
    in
    let%lwt () = Lwt_io.printl "Step 1 – set up your ships!" in
    let%lwt () = init_game 0 in
    let%lwt () = board_finished_waiter in

    (* turn taking *)
    check_p
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
