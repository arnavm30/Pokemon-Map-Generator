open State

exception Unfinished

let init x y num_tiles ws adj_rules = State.make x y num_tiles ws adj_rules

let run_once ws st =
  let c =
    match State.smallest_entropy st with
    | Some c -> c
    | None -> raise Unfinished
  in
  State.collapse_cell ws c st;
  State.propogate st

let rec run ws st =
  if st.uncollapsed = 0 then FINISHED st
  else
    match run_once ws st with
    | FINISHED st -> run ws st
    | CONTRADICTION -> CONTRADICTION

let rec wfc x y num_tiles ws adj_rules =
  let init_st = init x y num_tiles ws adj_rules in
  match run ws init_st with
  | FINISHED st -> st
  | CONTRADICTION -> wfc x y num_tiles ws adj_rules
