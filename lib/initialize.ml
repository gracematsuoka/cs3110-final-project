module Coord = struct
  type t = int * int

  let compare = compare (* uses the built-in polymorphic compare *)
end

module CoordSet = Set.Make (Coord)

type ship = {
  name : string;
  mutable coords : CoordSet.t;
}

type grid_state =
  | EMPTY
  | SHIP
  | HIT
  | MISS
  | SINK

let ship_list0 : ship list = [ { name = "0a"; coords = CoordSet.empty } ]
let ship_list1 : ship list = [ { name = "0a"; coords = CoordSet.empty } ]
let board_list : grid_state array array list = []
