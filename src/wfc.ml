open State

exception Unfinished

let init x y num_tiles ws adj_rules = State.make x y num_tiles ws adj_rules

let run_once num_tiles ws adj_rules st =
  let c =
    match State.smallest_entropy st with
    | Some c -> c
    (* this should be impossible *)
    | None -> raise Unfinished
  in
  State.collapse_cell ws c st;
  (* let valid, violations = State.check_valid adj_rules st in
     (* if not valid then (
        print_endline "Violations: ";
        List.iter (fun x -> print_endline x) violations;
        print_endline "Neighbors: ";
        State.print_neighbors c st;
        Cell.print_stats c;
        raise Unfinished); *) *)
  State.propogate num_tiles ws adj_rules st

let rec run num_tiles ws adj_rules st =
  try
    if st.uncollapsed = 0 then FINISHED st
    else
      match run_once num_tiles ws adj_rules st with
      | FINISHED st -> run num_tiles ws adj_rules st
      (* end this run if there is a contradiction *)
      | CONTRADICTION -> CONTRADICTION
  with Unfinished -> raise Unfinished (* CONTRADICTION *)

let rec wfc (x, y) num_tiles ws adj_rules =
  let init_st = init x y num_tiles ws adj_rules in
  match run num_tiles ws adj_rules init_st with
  | FINISHED st -> st
  (* start over again if there is a contradiction *)
  | CONTRADICTION -> wfc (x, y) num_tiles ws adj_rules
