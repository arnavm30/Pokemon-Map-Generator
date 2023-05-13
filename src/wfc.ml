open State

exception Unfinished

let init x y num_tiles ws adj_rules = State.make x y num_tiles ws adj_rules

let run_once ws st =
  let c =
    match State.smallest_entropy st with
    | Some c -> c
    (* this should be impossible *)
    | None -> raise Unfinished
  in
  State.collapse_cell ws c st;
  State.propogate st

let rec run ws st =
  try
    if st.uncollapsed = 0 then FINISHED st
    else
      match run_once ws st with
      | FINISHED st -> run ws st
      (* end this run if there is a contradiction *)
      | CONTRADICTION -> CONTRADICTION
  with Unfinished -> raise Unfinished (* CONTRADICTION *)

let rec wfc x y num_tiles ws adj_rules =
  let init_st = init x y (num_tiles - 1) ws adj_rules in
  match run ws init_st with
  | FINISHED st -> st
  (* start over again if there is a contradiction *)
  | CONTRADICTION -> wfc x y (num_tiles - 1) ws adj_rules
