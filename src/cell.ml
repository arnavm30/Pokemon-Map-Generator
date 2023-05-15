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
  if t.sum_of_ones = 0 then
    (* print_endline "We have contradiction";
       print_endline (Util.string_of_int_pair t.coords);
       print_endline (options_to_string t);
       print_endline (enablers_to_string t); *)
    raise Contradiction
  else ()

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

(* chooses a random option based on frequency *)
let choose_random_option ws t =
  let r = Random.float t.sum_of_weights |> ref in
  let i = ref 0 in
  while !i < Array.length t.options && !r >= 0. do
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
  t.options <- Array.make (Array.length t.options) 0.;
  t.options.(chosen) <- 1.;
  t.sum_of_ones <- 1;
  t.tile <- chosen;
  removed

let has_zero_direction tile t =
  let dirs = t.tile_enablers.(tile) in
  dirs.up <= 0 || dirs.left <= 0 || dirs.right <= 0 || dirs.down <= 0

let print_stats t =
  print_endline "Cell: ";
  print_endline (Util.string_of_int_pair t.coords);
  print_endline (options_to_string t);
  print_endline (enablers_to_string t);
  print_endline ("Sum of ones: " ^ string_of_int t.sum_of_ones);
  print_endline ("Tile: " ^ string_of_int t.tile);
  print_endline ""

let remove_tile ws tile t =
  (* print_endline "remove tile:";
     print_endline ("tile: " ^ string_of_int tile);
     print_stats t; *)
  t.options.(tile) <- 0.;
  t.sum_of_ones <- t.sum_of_ones - 1;
  (* print_endline "result:";
     print_stats t; *)
  t.sum_of_weights <- t.sum_of_weights -. ws.(tile);
  t.sum_of_weight_log_weights <-
    t.sum_of_weight_log_weights -. (ws.(tile) *. log ws.(tile))

let observe (i : int) (t : t) =
  t.options.(i) <- 0.;
  check_collapsed t

let options t = t.options
let to_string t = Util.string_of_int_pair t.coords
