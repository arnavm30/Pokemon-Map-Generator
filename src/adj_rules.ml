type directions = UP | DOWN | LEFT | RIGHT

module TileTriple = struct
  type t = int * int * directions

  let compare a b =
    match (a, b) with
    | (w, x, d1), (y, z, d2) ->
        if w = y && x = z && d1 = d2 then 0
        else
          let c = compare w y in
          if c = 0 then -1 else c
end

module AdjSet = Set.Make (TileTriple)

type t = AdjSet.t

let empty = AdjSet.empty
let allow a b dir s = AdjSet.add (a, b, dir) s

(* Goes in order of UP, DOWN, LEFT, RIGHT *)
let fold_dirs f acc = acc |> f UP |> f DOWN |> f LEFT |> f RIGHT
let allow_all a b s = fold_dirs (fun dir acc -> AdjSet.add (a, b, dir) acc) s
let is_allowed a b dir s = AdjSet.mem (a, b, dir) s
