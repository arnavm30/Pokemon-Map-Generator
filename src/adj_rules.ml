type directions = UP | DOWN | LEFT | RIGHT

(* module TileTriple = struct
     type t = int * int * directions
     (* (a, b, dir) implies that b is in the dir of a, i.e. (0, 1, UP) means that tile 1 is above 0 *)

     let compare a b =
       match (a, b) with
       | (w, x, d1), (y, z, d2) -> if w = y && x = z && d1 = d2 then 0 else 1
     (* let c = compare w y in
        if c = 0 then -1 else 1 *)
   end *)

(* module AdjSet = Set.Make (TileTriple) *)

type t = (int * int * directions, unit) Hashtbl.t

let string_of_dir dir =
  match dir with
  | UP -> "up"
  | DOWN -> "down"
  | LEFT -> "left"
  | RIGHT -> "right"

let empty () = Hashtbl.create 10

let allow a b dir ht =
  Hashtbl.add ht (a, b, dir) ();
  ht

let combine ht1 ht2 =
  if Hashtbl.length ht1 < Hashtbl.length ht2 then (
    Hashtbl.iter
      (fun x _ -> if not (Hashtbl.mem ht2 x) then Hashtbl.add ht2 x ())
      ht1;
    ht2)
  else (
    Hashtbl.iter
      (fun x _ -> if not (Hashtbl.mem ht1 x) then Hashtbl.add ht1 x ())
      ht2;
    ht1)

(* Goes in order of UP, DOWN, LEFT, RIGHT *)
let fold_dirs f acc = acc |> f UP |> f DOWN |> f LEFT |> f RIGHT
let fold_dirs_tile_indexed f t acc = fold_dirs (f t) acc

let allow_all a b ht =
  fold_dirs
    (fun dir acc ->
      Hashtbl.add ht (a, b, dir) ();
      ht)
    ht

let is_allowed a b dir ht = Hashtbl.mem ht (a, b, dir)

let get_all_allowed a t s =
  let rec aux t dir acc =
    if t < 0 then acc
    else if is_allowed a t dir s then aux (t - 1) dir ((a, t, dir) :: acc)
    else aux (t - 1) dir acc
  in
  fold_dirs_tile_indexed aux t []

let count_enablers_for_one a t s =
  let open Cell in
  let rec aux t dir acc =
    (* print_endline ("a:" ^ string_of_int a);
       print_endline ("t:" ^ string_of_int t);
       print_endline ("d:" ^ string_of_dir dir); *)
    (* print_endline (string_of_bool (is_allowed 3 1 UP s)); *)
    if t < 0 then acc
    else if is_allowed a t dir s then
      match dir with
      | UP -> aux (t - 1) dir { acc with up = acc.up + 1 }
      | DOWN -> aux (t - 1) dir { acc with down = acc.down + 1 }
      | LEFT -> aux (t - 1) dir { acc with left = acc.left + 1 }
      | RIGHT -> aux (t - 1) dir { acc with right = acc.right + 1 }
    else aux (t - 1) dir acc
  in
  fold_dirs_tile_indexed aux t { up = 0; down = 0; left = 0; right = 0 }

let count_init_enablers t s =
  let counts =
    Array.make (t + 1) Cell.{ up = 0; down = 0; left = 0; right = 0 }
  in
  let rec aux a =
    if a < 0 then counts
    else
      let _ = counts.(a) <- count_enablers_for_one a t s in
      aux (a - 1)
  in
  aux t

let opposite_dir dir =
  match dir with UP -> DOWN | DOWN -> UP | LEFT -> RIGHT | RIGHT -> LEFT
(* let get_allowed a b dir s =
   let *)

let cmp (w, x, d1) (y, z, d2) =
  if w < y then -1
  else if w = y then if x < z then -1 else if x = z then 0 else 1
  else 1

let get_all_rules ht =
  let elem_lst = List.sort cmp (Hashtbl.fold (fun x _ acc -> x :: acc) ht []) in
  List.map (fun elem -> match elem with i, j, dir -> (i, j, dir)) elem_lst

let print_to_string t =
  List.iter
    (fun (i, j, dir) ->
      let str =
        ("(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", "
        ^
        match dir with
        | UP -> "up"
        | DOWN -> "down"
        | LEFT -> "left"
        | RIGHT -> "right")
        ^ ")"
      in
      print_endline str)
    (List.sort cmp (Hashtbl.fold (fun x _ acc -> x :: acc) t []))
