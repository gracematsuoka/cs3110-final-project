For this project's GUI, we are using GTK3. To use GTK3 do the following:

- Install GTK3 on your machine (C library): `brew install gtk+3`
- Install the GTK3 OCaml package: `opam install lablgtk3`

To run + build this dune project:

- Run `dune build`
- To start the project, you must build a server and connect clients:
  - Run `dune exec bin/main.exe server <IP Address>` to build the server
  - Run `dune exec bin/main.exe client <username> <IP Address>` to connect a client with a username to a specific
