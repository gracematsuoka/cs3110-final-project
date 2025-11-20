To run + build this dune project:

1. We first need to find what the IP Address of the computer where the server will built is.

- Run `ifconfig | grep "inet[^6]"` in your terminal
  It will return something like this:
  inet 127.0.0.1 netmask 0xff000000
  inet 10.48.123.0 netmask 0xffff0000 broadcast 10.48.255.255
  The IP address is the second inet address, so the example above has an IP address `10.48.123.0`
  This will be the IP address you will use to build the server

2. Open the project and go to the root directory of the project

- Run `dune build`
- To start the project, you must build a server and connect clients:
  - Run `dune exec bin/main.exe server <IP Address> <Port>` to build the server (Port can be any unused port number, like `500`)
  - Run `dune exec bin/main.exe client <username> <IP Address>` to connect a client with a username to a specific
    - The username can have a space AS LONG AS the whole username is wrapped around `"`

IGNORE THIS FOR NOW:
For this project's GUI, we are using GTK3. To use GTK3 do the following:

- Install GTK3 on your machine (C library): `brew install gtk+3`
- Install the GTK3 OCaml package: `opam install lablgtk3`
