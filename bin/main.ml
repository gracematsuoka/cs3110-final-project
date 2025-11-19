open Gtk

let () =
  let _ = GMain.init () in

  (* Main window *)
  let window =
    GWindow.window ~title:"Battleship" ~border_width:10 ~width:900 ~height:600
      ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  (* Horizontal box to hold two tables *)
  let hbox = GPack.hbox ~spacing:20 ~packing:window#add () in

  (* Create left table *)
  let left_table =
    GPack.table ~rows:10 ~columns:10 ~homogeneous:true
      ~packing:(fun w -> hbox#pack ~expand:true ~fill:true w)
      ()
  in

  (* Create right table *)
  let right_table =
    GPack.table ~rows:10 ~columns:10 ~homogeneous:true
      ~packing:(fun w -> hbox#pack ~expand:true ~fill:true w)
      ()
  in

  (* Fill left table with blank buttons *)
  for row = 0 to 9 do
    for col = 0 to 9 do
      ignore
        (GButton.button ~label:""
           ~packing:(fun w ->
             left_table#attach ~left:col ~top:row ~expand:`BOTH ~fill:`BOTH w)
           ())
    done
  done;

  (* Fill right table with blank buttons *)
  for row = 0 to 9 do
    for col = 0 to 9 do
      ignore
        (GButton.button ~label:""
           ~packing:(fun w ->
             right_table#attach ~left:col ~top:row ~expand:`BOTH ~fill:`BOTH w)
           ())
    done
  done;

  window#show ();
  GMain.main ()

(* open Cs3110_final_project.Initialize

   let player0personal = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |] let
   player0attack = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |] let
   player1personal = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |] let
   player1attack = [| [| EMPTY; EMPTY |]; [| EMPTY; EMPTY |] |]

   let board_list : grid_state array array list = [ player0personal;
   player0attack; player1personal; player1attack ]

   let localhost_5000 = Unix.ADDR_INET (Unix.inet_addr_loopback, 5000) let
   counter = ref 0 let client_output_channels : Lwt_io.output_channel list ref =
   ref [] let turn = ref 0

   (** [string_of_addr addr] converts a Unix address into a string *) let
   string_of_addr = function | Unix.ADDR_UNIX s -> s | ADDR_INET (ip, port) ->
   Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

   (** [fatal_error msg] will print an error message [msg] and then end the
   execution *) let fatal_error msg = prerr_endline msg; exit 1

   (** [parse_client_username] will parse the username argument of a client *)
   let parse_client_username () = if Array.length Sys.argv <> 3 then fatal_error
   "Please enter a username."; if String.starts_with ~prefix:"\"" Sys.argv.(1)
   then String.sub Sys.argv.(2) 1 (String.length Sys.argv.(2) - 2) else
   Sys.argv.(2)

   (** [notify_all message output_channel] will notify all the clients
   [message]*) let notify_all message output_channel = let%lwt () =
   Lwt_io.fprintlf output_channel "%s" message in let%lwt () = Lwt_io.flush
   output_channel in Lwt.return ()

   let rec read_print_n_lines n output_channel = if n = 0 then Lwt.return ()
   else let%lwt line = Lwt_io.read_line output_channel in let%lwt () =
   Lwt_io.printlf "%s" line in read_print_n_lines (n - 1) output_channel

   let client_handler client_addr (client_in, client_out) : unit Lwt.t = (*
   connecting to the client *) let address_string = string_of_addr client_addr
   in let%lwt client_username = Lwt_io.read_line client_in in let%lwt () =
   Lwt_io.printlf "I got a connection from %s (%s)." address_string
   client_username in

   (* notifying everyone of the connection *) let%lwt () = Lwt_io.fprintf
   client_out "You joined the game\n" in let%lwt () = Lwt_io.flush client_out in
   let%lwt () = Lwt_list.iter_p (notify_all (client_username ^ " joined the
   game")) !client_output_channels in client_output_channels := client_out ::
   !client_output_channels; incr counter; let%lwt () = let status =
   Printf.sprintf "Currently have %d/%d players" !counter 2 in

   let message = if !counter > 1 then status ^ "\nStarting game" else
   Printf.sprintf "%s\nWaiting for %d more player(s)..." status (2 - !counter)
   in

   Lwt_list.iter_p (notify_all message) !client_output_channels in let%lwt () =
   Lwt_io.flush client_out in let%lwt () = Lwt_io.flush client_out in

   (* turn taking *) let rec receive_message () = Lwt.catch (fun () -> let%lwt
   message = Lwt_io.read_line client_in in let%lwt () = Lwt_io.printlf "Message
   received from client %s. %s" address_string message in let%lwt () =
   Lwt_list.iter_p (notify_all (client_username ^ " says: " ^ message))
   (List.filter (fun output_channel -> output_channel != client_out)
   !client_output_channels) in receive_message ()) (function | End_of_file ->
   client_output_channels := List.filter (fun output_channel -> output_channel
   != client_out) !client_output_channels; let%lwt () = Lwt_list.iter_p
   (notify_all (client_username ^ " left the chat")) !client_output_channels in
   Lwt_io.printlf "Client %s (%s) disconnected." address_string client_username
   | exn -> fatal_error ("Error: " ^ Printexc.to_string exn)) in receive_message
   ()

   let run_server () = let server () = let%lwt () = Lwt_io.printlf "Server built
   successfully." in let%lwt running_server =
   Lwt_io.establish_server_with_client_address localhost_5000 client_handler in
   let (never_resolved : unit Lwt.t), _unused_resolver = Lwt.wait () in
   never_resolved in Lwt_main.run (server ())

   let run_client () = let client () = (* establishing usernames and connecting
   to server *) let game_started_waiter, wake_game_started = Lwt.wait () in let
   client_username = parse_client_username () in let%lwt server_in, server_out =
   Lwt_io.open_connection localhost_5000 in let%lwt () = Lwt_io.write_line
   server_out client_username in let%lwt () = Lwt_io.flush server_out in

   (* initialize game *) let rec send () = let%lwt () = Lwt_io.print "Send a
   message: " in let%lwt message_opt = Lwt_io.read_line_opt Lwt_io.stdin in
   match message_opt with | None -> fatal_error "Input closed. Exiting client."
   | Some message -> Lwt.catch (fun () -> let%lwt () = Lwt_io.write_line
   server_out message in let%lwt () = Lwt_io.flush server_out in send ())
   (function | Lwt_io.Channel_closed _ | Unix.Unix_error (_, _, _) ->
   fatal_error "Server disconnected." | exn -> fatal_error ("Error: " ^
   Printexc.to_string exn)) in

   let rec check_server () = let%lwt message_opt = Lwt_io.read_line_opt
   server_in in match message_opt with | None -> fatal_error "\nServer
   disconnected." | Some msg -> let%lwt () = Lwt_io.printlf "%s" msg in if msg =
   "Starting game" then (* wake the promise once *) Lwt.wakeup_later
   wake_game_started (); check_server () in let check_p = check_server () in
   let%lwt () = game_started_waiter in Lwt.choose [ send (); check_p ] in
   Lwt_main.run (client ())

   let _ = let print_usage () = Printf.printf "Please specify whether you are a
   server or client" in if Array.length Sys.argv < 1 then print_usage () else
   match Sys.argv.(1) with | "server" -> run_server () | "client" -> run_client
   () | _ -> print_usage () *)
