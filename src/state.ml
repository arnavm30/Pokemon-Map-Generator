open Cell

type t = {
  grid : Cell.t array array;
  heap : Cell.t Pairing_heap.t;
  stack : (Cell.t * int) Stack.t;
  w : int;
  h : int;
  mutable uncollapsed : int;
}

type result = FINISHED of t | CONTRADICTION

let init_weight_sums ws =
  let aux acc x =
    match acc with sw, swlw -> (x +. sw, (x *. log x) +. swlw)
  in
  Array.fold_left aux (0., 0.) ws

let init_tile_enablers t adj = Adj_rules.count_init_enablers t adj

let make_grid x y c =
  let give_coords y x = c (x, y) in
  let make_row x y = Array.init x (give_coords y) in
  Array.init y (make_row x)

let make (x : int) (y : int) (num_tiles : int) (ws : float array)
    (adj : Adj_rules.t) =
  let sw, swlw = init_weight_sums ws in
  let enablers = init_tile_enablers (num_tiles - 1) adj in
  let grid = make_grid x y (Cell.make num_tiles sw swlw enablers) in
  (* let grid = make_grid x y (Cell.make t sw swlw enablers) in *)
  let heap = Pairing_heap.create ~min_size:(x * y) ~cmp:Cell.cmp () in
  let stack = Stack.create () in
  Array.iter (Array.iter (Pairing_heap.add heap)) grid;
  print_endline (Cell.enablers_to_string grid.(0).(0));
  { grid; heap; stack; w = x; h = y; uncollapsed = x * y }

let rec smallest_entropy st =
  match Pairing_heap.pop st.heap with
  (* avoid choosing an already collapsed cell*)
  | Some c -> if c.collapsed then smallest_entropy st else Some c
  (* impossible *)
  | None -> None

let collapse_cell ws c st =
  st.uncollapsed <- st.uncollapsed - 1;
  let removed = Cell.collapse ws c in
  (* push every removed tile to the stack *)
  List.iter (fun t -> Stack.push (c, t) st.stack) removed

let get_neighbors c st =
  let open Adj_rules in
  let x, y = c.coords in
  let neighbors = [] in
  (* there's probably a way to make this not terrible *)
  let neighbors =
    if x > 0 then (LEFT, (x - 1, y)) :: neighbors else neighbors
  in
  let neighbors =
    if x < st.w - 1 then (RIGHT, (x + 1, y)) :: neighbors else neighbors
  in
  let neighbors = if y > 0 then (UP, (x, y - 1)) :: neighbors else neighbors in
  if y < st.h - 1 then (DOWN, (x, y + 1)) :: neighbors else neighbors

let decr_dir t dir c =
  let open Adj_rules in
  match dir with
  | UP -> c.tile_enablers.(t).up <- c.tile_enablers.(t).up - 1
  | DOWN -> c.tile_enablers.(t).down <- c.tile_enablers.(t).down - 1
  | LEFT -> c.tile_enablers.(t).left <- c.tile_enablers.(t).left - 1
  | RIGHT -> c.tile_enablers.(t).right <- c.tile_enablers.(t).right - 1

let enablers_in_direction t dir c =
  let open Adj_rules in
  match dir with
  | UP -> c.tile_enablers.(t).up
  | DOWN -> c.tile_enablers.(t).down
  | LEFT -> c.tile_enablers.(t).left
  | RIGHT -> c.tile_enablers.(t).right

let get_enabled num_tiles tile dir adj_rules n =
  let enabled = ref [] in
  for i = 0 to num_tiles - 1 do
    if Adj_rules.is_allowed i tile dir adj_rules then enabled := i :: !enabled
  done;
  !enabled

let subtract_enablers num_tiles tile dir ws adj_rules n st =
  let opp_dir = Adj_rules.opposite_dir dir in
  let enabled = get_enabled num_tiles tile opp_dir adj_rules n in
  let rec aux enabled =
    match enabled with
    | [] -> ()
    | h :: t ->
        let enablers = enablers_in_direction h opp_dir n in
        if
          enablers = 1 && (not (Cell.has_zero_direction h n)) && not n.collapsed
        then (
          Cell.remove_tile ws h n;
          Cell.check_contradiction n;
          Pairing_heap.add st.heap n;
          Stack.push (n, h) st.stack);
        decr_dir h opp_dir n;
        aux t
  in
  aux enabled
(* print_endline "subtract enablers: ";
   print_endline ("tile: " ^ string_of_int tile);
   print_endline ("dir: " ^ Adj_rules.string_of_dir dir);
   print_endline ("n: " ^ Util.string_of_int_pair n.coords);
   print_endline (Cell.enablers_to_string n);
   print_endline "" *)

let rec propogate (num_tiles : int) (ws : float array) (adj_rules : Adj_rules.t)
    (st : t) =
  try
    (* using pop allows us to exit early *)
    let c, tile = Stack.pop st.stack in
    let neighbors = get_neighbors c st in
    let rec process_neighbors ns =
      match ns with
      | [] -> ()
      | (dir, (x, y)) :: t ->
          subtract_enablers num_tiles tile dir ws adj_rules st.grid.(y).(x) st;
          process_neighbors t
    in
    process_neighbors neighbors;
    propogate num_tiles ws adj_rules st
  with
  | Stack.Empty -> FINISHED st
  | Cell.Contradiction -> CONTRADICTION

let check_valid adj_rules st =
  let valid = ref true in
  let violations = ref [] in
  for y = 0 to Array.length st.grid - 1 do
    for x = 0 to Array.length st.grid.(0) - 1 do
      let open Adj_rules in
      let tile = st.grid.(y).(x).tile in
      if tile <> -1 then (
        if x > 0 && st.grid.(y).(x - 1).tile <> -1 then
          if
            not
              (Adj_rules.is_allowed tile st.grid.(y).(x - 1).tile LEFT adj_rules)
          then (
            valid := false;
            violations :=
              ("("
              ^ Util.string_of_int_pair (x - 1, y)
              ^ ", "
              ^ Adj_rules.string_of_dir LEFT
              ^ ")")
              :: !violations);
        if x < st.w - 1 && st.grid.(y).(x + 1).tile <> -1 then
          if
            not
              (Adj_rules.is_allowed tile st.grid.(y).(x + 1).tile RIGHT
                 adj_rules)
          then (
            valid := false;
            violations :=
              ("("
              ^ Util.string_of_int_pair (x + 1, y)
              ^ ", "
              ^ Adj_rules.string_of_dir RIGHT
              ^ ")")
              :: !violations);
        if y > 0 && st.grid.(y - 1).(x).tile <> -1 then
          if
            not
              (Adj_rules.is_allowed tile st.grid.(y - 1).(x).tile UP adj_rules)
          then (
            valid := false;
            violations :=
              ("("
              ^ Util.string_of_int_pair (x, y - 1)
              ^ ", " ^ Adj_rules.string_of_dir UP ^ ")")
              :: !violations);
        if y < st.h - 1 && st.grid.(y + 1).(x).tile <> -1 then
          if
            not
              (Adj_rules.is_allowed tile st.grid.(y + 1).(x).tile DOWN adj_rules)
          then (
            valid := false;
            violations :=
              ("("
              ^ Util.string_of_int_pair (x, y + 1)
              ^ ", "
              ^ Adj_rules.string_of_dir DOWN
              ^ ")")
              :: !violations))
    done
  done;
  (!valid, !violations)

let validate adj_rules st =
  let valid, _ = check_valid adj_rules st in
  if valid then print_endline "valid" else print_endline "not valid"

let draw (st : t) (x, y) (tiles : Tile.t array) =
  for i = 0 to Array.length st.grid - 1 do
    for j = 0 to Array.length st.grid.(0) - 1 do
      let index = st.grid.(i).(j).tile in
      let img = Tile.get_img tiles.(index) in
      let img_color_array = Graphics.dump_image img in
      let img_width = Array.length img_color_array in
      let img_height = Array.length img_color_array.(0) in
      Graphics.draw_image img (x + (img_width * j)) (y + (img_height * i))
    done
  done

let apply_to_neighbors f c st acc =
  let open Adj_rules in
  let x, y = c.coords in
  let grid = st.grid in
  let acc = if y > 0 then f grid.(y - 1).(x) UP acc else acc in
  let acc = if y < st.h - 1 then f grid.(y + 1).(x) DOWN acc else acc in
  let acc = if x > 0 then f grid.(y).(x - 1) LEFT acc else acc in
  if x < st.w - 1 then f grid.(y).(x + 1) RIGHT acc else acc

let iter_neighbors f c st = apply_to_neighbors (fun x dir _ -> f x dir) c st ()

let print_neighbors c st =
  let print_neighbors n dir =
    print_endline ("Neighbor Dir: " ^ Adj_rules.string_of_dir dir);
    Cell.print_stats n
  in
  iter_neighbors print_neighbors c st

let print_tiles st =
  let grid = st.grid in
  for i = 0 to Array.length grid - 1 do
    Array.iter
      (fun cell -> print_string (string_of_int cell.tile ^ " "))
      grid.(i);
    print_endline ""
  done
