open Util
open Graphics
open Random

module Tile = struct
  type t = {
    mutable collapsed : bool;
    options : float array;
    mutable sum_of_ones : int;
        (* mutable sum_of_weights: float;
           mutable sum_of_logweights: float;
           weights: float array; *)
  }
  (** Representation invariant: *)

  let check_collapsed t = if sumf t.options <= 1. then t.collapsed <- true

  let make (l : int) =
    { collapsed = false; options = Array.make l 1.; sum_of_ones = l }

  let make_test (l : int) =
    self_init ();
    let rnd_index = Random.float 4. in
    { collapsed = true; options = Array.make 1 rnd_index; sum_of_ones = l }

  let observe (i : int) (t : t) =
    t.options.(i) <- 0.;
    check_collapsed t

  let options t = t.options
end

open Tile

type t = Tile.t array array

let make (x : int) (y : int) (l : int) = Array.make_matrix x y (Tile.make l)

let make_test (x : int) (y : int) (l : int) =
  Array.make_matrix x y (Tile.make_test l)

let smallest_entropies (st : t) (w : float array) =
  (* let aux (arr: Tile.t array) =
       let calc_entropy (agg: float * float list) (t:Tile.t) =
         let e = entropy(w ***. t.options) in
         match agg with
         | min, mins -> if e < min then (e, []) else if e = min then (min, )
       Array.fold_left (fun lst t -> if t.options) (Float.max_float, []) arr
     let min = Float.min_float in let mins = [] in
     Array.fold_left (fun arr arr -> ) *)
  let min = ref Float.max_float in
  let mins = ref [] in

  for x = 0 to Array.length st do
    for y = 0 to Array.length st.(x) do
      let e = entropy (st.(x).(y).options ***. w) in
      let m = !min in
      (* because dereferencing can be expensive *)
      if e < m then (
        min := e;
        mins := [])
      else if e = m then mins := (x, y) :: !mins
    done
  done;

  !mins

let smallest_entropy (st : t) (w : float array) =
  let smallest = smallest_entropies st w in
  try smallest |> List.length |> Random.int |> List.nth smallest
  with Failure _ -> List.hd smallest

let propogate (st : t) = failwith "not implemented"

let draw (st : t) (x : int) (y : int) =
  open_graph "";
  for i = 0 to Array.length st - 1 do
    for j = 0 to Array.length st.(0) - 1 do
      let index = int_of_float st.(i).(j).options.(0) in
      let img = Png.load ("assets/corner" ^ string_of_int index ^ ".png") [] in
      let g = Graphic_image.of_image img in
      let g_color_array = dump_image g in
      let img_width = Array.length g_color_array in
      let img_height = Array.length g_color_array.(0) in
      Graphics.draw_image g (x + (img_width * i)) (y + (img_height * j))
    done
  done;
  ignore (read_key ())
