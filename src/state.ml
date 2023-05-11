open Cell
open Pairing_heap

type t = { state : Cell.t array array; heap : Cell.t Pairing_heap.t }

let init_weight_sums ws =
  let aux acc x =
    match acc with sw, swlw -> (x +. sw, (x *. log x) +. swlw)
  in
  Array.fold_left aux (0., 0.) ws

let count_enablers t num_tiles adj =
  let open! Adj_rules in
  let rec aux dir acc =
    match acc with
    | n, (u, d, l, r) -> (
        if n < 0 then acc
        else if not (is_allowed t n dir adj) then aux dir (n - 1, (u, d, l, r))
        else
          match dir with
          | UP -> aux dir (n - 1, (u + 1, d, l, r))
          | DOWN -> (n - 1, (u, d + 1, l, r))
          | LEFT -> (n - 1, (u, d, l + 1, r))
          | RIGHT -> (n - 1, (u, d, l, r + 1)))
  in
  Adj_rules.fold_dirs aux (num_tiles, (0, 0, 0, 0))

let init_tile_enablers t num_tiles adj = count_enablers t num_tiles adj

(* let make_test (l : int) =
   { collapsed = true; options = Array.make l 1.; sum_of_ones = 1 } *)

let make (x : int) (y : int) (l : int) (ws : float array) (adj : Adj_rules.t) =
  let sw, swlw = init_weight_sums ws in
  let state = Array.make_matrix x y (Cell.make l sw swlw) in
  let heap = create ~min_size:(x * y) ~cmp () in
  Array.iter (Array.iter (Pairing_heap.add heap)) state;
  { state; heap }

let make_test (x : int) (y : int) (tiles : Tile.t array) =
  {
    state = Array.make_matrix x y (Cell.make_test tiles);
    heap = Pairing_heap.create ~min_size:0 ~cmp:(fun a b -> 0) ();
  }

(* Array.make_matrix x y (Tile.make_test (Array.length cells)) *)

let rec smallest_entropy st =
  match pop st.heap with
  | Some c -> if c.collapsed then Some c else smallest_entropy st
  | None -> None

(* let smallest_entropies (st : t) (w : float array) =
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

   for x = 0 to Array.length st.state do
     for y = 0 to Array.length st.state.(x) do
       let e = entropy (st.state.(x).(y).options ***. w) in
       let m = !min in
       (* because dereferencing can be expensive *)
       if e < m then (
         min := e;
         mins := [])
       else if e = m then mins := (x, y) :: !mins
     done
   done;

   !mins *)
(*
   let smallest_entropy (st : t) (w : float array) =
     let smallest = smallest_entropies st w in
     try smallest |> List.length |> Random.int |> List.nth smallest
     with Failure _ -> List.hd smallest *)

let propogate (st : t) (tiles : Tile.t array) = failwith "unimplemented"
(* let (i,j) = smallest_entropy st (Array.make (Array.length cells) 1.) in
   let tile = st.(i).(j) in *)

let draw (st : t) (x : int) (y : int) (cells : Tile.t array) =
  for i = 0 to Array.length st.state - 1 do
    for j = 0 to Array.length st.state.(0) - 1 do
      let index = int_of_float st.state.(i).(j).options.(0) in
      let img = Tile.get_img cells.(index) in
      let img_color_array = Graphics.dump_image img in
      let img_width = Array.length img_color_array in
      let img_height = Array.length img_color_array.(0) in
      Graphics.draw_image img (x + (img_width * i)) (y + (img_height * j))
    done
  done;
  ignore (Graphics.read_key ())
