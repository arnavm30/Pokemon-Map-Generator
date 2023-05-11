open Util

type directions = { up : int; right : int; down : int; left : int }

type t = {
  mutable collapsed : bool;
  options : float array;
  mutable sum_of_ones : int;
  mutable sum_of_weights : float;
  mutable sum_of_weight_log_weights : float;
  noise : float;
  tile_enablers : directions;
}
(** Representation invariant: *)

let entropy t =
  log t.sum_of_weights
  -. (t.sum_of_weight_log_weights /. t.sum_of_weights)
  +. t.noise

let cmp a b = if entropy a <= entropy b then -1 else 1
let check_collapsed t = if sumf t.options <= 1. then t.collapsed <- true

(* in theory, these are all the same at the beginning *)
let make (l : int) (w : float) (lw : float) =
  {
    collapsed = false;
    options = Array.make l 1.;
    sum_of_ones = l;
    sum_of_weights = w;
    sum_of_weight_log_weights = lw;
    noise = Random.float 0.0000001;
    tile_enablers = { up = l; right = l; down = l; left = l };
  }

let make_test (cells : Cells.t array) =
  Random.self_init ();
  let rnd_index = Random.float 4. in
  {
    collapsed = true;
    options = [| rnd_index |];
    sum_of_ones = 1;
    sum_of_weights = 1.;
    sum_of_weight_log_weights = 1.;
    noise = 0.;
    tile_enablers = { up = 1; right = 1; down = 1; left = 1 };
  }

(* chooses a random option based on frequency *)
let choose_random_option t =
  let r = Random.float t.sum_of_weights |> ref in
  let i = ref 0 in
  let weights = !Main.weights in
  while !i < Array.length weights && !r >= 0. do
    incr i;
    r := !r -. weights.(!i)
  done;
  !i

let collapse t = choose_random_option t

let observe (i : int) (t : t) =
  t.options.(i) <- 0.;
  check_collapsed t

let options t = t.options
