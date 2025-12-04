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
  - Run `dune exec bin/main.exe server <IP Address> <Port>` to build the server (Port can be any unused port number, like `5000`)
  - If you want to play a 2-player game:
    - Run `dune exec bin/main.exe client <IP Address> <Port> <name>` to connect a client with a name to a server
      - The username can have a space AS LONG AS the whole username is wrapped around `"`
  - If you want to play a 1-player game:
    - Run `dune exec bin/main.exe client <IP Address> <Port> <name> AI` to connect a client with a name to a server that will be playing against an AI player
