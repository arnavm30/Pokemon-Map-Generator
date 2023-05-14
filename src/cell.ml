type directions = {
  mutable up : int;
  mutable right : int;
  mutable down : int;
  mutable left : int;
}

type t = {
  mutable collapsed : bool;
  mutable tile : int;
  mutable options : float array;
  mutable sum_of_ones : int;
  mutable sum_of_weights : float;
  mutable sum_of_weight_log_weights : float;
  noise : float;
  tile_enablers : directions array;
  coords : int * int;
}

exception Contradiction
(** Representation invariant: *)

let options_to_string t =
  let str = ref "[" in
  for i = 0 to Array.length t.options - 1 do
    str := !str ^ string_of_float t.options.(i)
  done;
  !str ^ "]"

let enabler_dirs_to_string dirs =
  "{up: " ^ string_of_int dirs.up ^ " down: " ^ string_of_int dirs.down
  ^ " left: " ^ string_of_int dirs.left ^ " right: " ^ string_of_int dirs.right
  ^ "}"

let enablers_to_string t =
  let str = ref "{\n" in
  for i = 0 to Array.length t.tile_enablers - 1 do
    str :=
      !str ^ "\n\t" ^ string_of_int i ^ ":"
      ^ enabler_dirs_to_string t.tile_enablers.(i)
  done;
  !str ^ "\n}"

let entropy t =
  log t.sum_of_weights
  -. (t.sum_of_weight_log_weights /. t.sum_of_weights)
  +. t.noise

let cmp a b = if entropy a <= entropy b then -1 else 1
let check_collapsed t = if t.sum_of_ones = 1 then t.collapsed <- true

let check_contradiction t =
  if t.sum_of_ones = 0 then raise Contradiction else ()

let copy_enablers enablers =
  let copy_directions { up; down; left; right } = { up; down; left; right } in
  let copy =
    Array.make (Array.length enablers) { up = 0; down = 0; left = 0; right = 0 }
  in
  Array.iteri (fun i x -> copy.(i) <- copy_directions x) enablers;
  copy

(* in theory, these are all the same at the beginning *)
let make (num_tiles : int) (sw : float) (swlw : float)
    (enablers : directions array) (coords : int * int) =
  {
    collapsed = false;
    tile = -1;
    options = Array.make num_tiles 1.;
    sum_of_ones = num_tiles;
    sum_of_weights = sw;
    sum_of_weight_log_weights = swlw;
    noise = Random.float 0.0000001;
    tile_enablers = copy_enablers enablers;
    coords;
  }

let make_test (tiles : Tile.t array) =
  Random.self_init ();
  let rnd_index = Random.float 4. in
  {
    collapsed = true;
    tile = -1;
    options = [| rnd_index |];
    sum_of_ones = 1;
    sum_of_weights = 1.;
    sum_of_weight_log_weights = 1.;
    noise = 0.;
    tile_enablers = [| { up = 1; right = 1; down = 1; left = 1 } |];
    coords = (0, 0);
  }

(* chooses a random option based on frequency *)
let choose_random_option ws t =
  let r = Random.float t.sum_of_weights |> ref in
  let i = ref 0 in
  while !i < Array.length t.options && !r >= 0. do
    (* print_endline "length: ";
       print_endline (string_of_int (Array.length t.options));
       print_endline "options: ";
       print_endline (string_of_float t.options.(!i));
       print_endline "weights: ";
       print_endline (string_of_float ws.(!i)); *)
    if t.options.(!i) = 1. then r := !r -. ws.(!i);
    incr i
  done;
  (* to account for the last increment in the loop *)
  !i - 1

let collapse ws t =
  t.collapsed <- true;
  let chosen = choose_random_option ws t in
  let aux acc x =
    let i, lst = acc in
    if x = 1. && i <> chosen then (i + 1, i :: lst) else (i + 1, lst)
  in
  let _, removed = Array.fold_left aux (0, []) t.options in
  (* print_endline "options: ";
     Array.iter (fun x -> print_string (string_of_float x ^ " ")) t.options;
     print_endline (string_of_int chosen);
     print_endline "removed: ";
     List.iter (fun x -> print_string (string_of_int x ^ " ")) removed; *)
  (* if t.coords = (0, 0) then (
       print_endline ("first: " ^ options_to_string t);
       print_endline (enablers_to_string t));
     if t.coords = (1, 0) then (
       print_endline ("second: " ^ options_to_string t);
       print_endline (enablers_to_string t)); *)
  t.options <- Array.make (Array.length t.options) 0.;
  t.options.(chosen) <- 1.;
  t.sum_of_ones <- 1;
  t.tile <- chosen;
  removed

let has_zero_direction tile t =
  let dirs = t.tile_enablers.(tile) in
  dirs.up <= 0 || dirs.left <= 0 || dirs.right <= 0 || dirs.down <= 0

let remove_tile ws tile t =
  t.options.(tile) <- 0.;
  print_endline "hello";
  t.sum_of_ones <- t.sum_of_ones - 1;
  t.sum_of_weights <- t.sum_of_weights -. ws.(tile);
  t.sum_of_weight_log_weights <-
    t.sum_of_weight_log_weights -. (ws.(tile) *. log ws.(tile))

(* let decr_dir tile dir t =
   let open Adj_rules in
   match dir with
   | UP -> t.tile_enablers.(tile).up <- t.tile_enablers.(tile).up - 1
   | DOWN -> t.tile_enablers.(tile).down <- t.tile_enablers.(tile).down - 1
   | LEFT -> t.tile_enablers.(tile).left <- t.tile_enablers.(tile).left - 1
   | RIGHT -> t.tile_enablers.(tile).right <- t.tile_enablers.(tile).right - 1 *)

let observe (i : int) (t : t) =
  t.options.(i) <- 0.;
  check_collapsed t

let options t = t.options
let to_string t = Util.string_of_int_pair t.coords
