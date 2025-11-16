module Coord = struct
  type t = int * int

  let compare = compare
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

(* temperorary values, change when implementing this file!! *)
let ship_list0_upd : ship list = [ { name = "0a"; coords = CoordSet.empty } ]
let ship_list1_upd : ship list = [ { name = "0a"; coords = CoordSet.empty } ]
let ship_list0_og : ship list = [ { name = "0a"; coords = CoordSet.empty } ]
let ship_list1_og : ship list = [ { name = "0a"; coords = CoordSet.empty } ]
let board_list : grid_state array array list = []
