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

(* let count_enablers c num_tiles adj =
   Adj_rules.count_enablers c (num_tiles - 1) adj *)
(* let rec aux dir acc =
   match acc with
   | n, ({ up; down; left; right } as dirs) -> (
       if n < 0 then (num_tiles - 1, dirs)
         (* reset the tile counter to loop again *)
       else if not (is_allowed c n dir adj) then aux dir (n - 1, dirs)
       else
         match dir with
         | UP -> aux dir (n - 1, { dirs with up = up + 1 })
         | DOWN -> (n - 1, { dirs with down = down + 1 })
         | LEFT -> (n - 1, { dirs with left = left + 1 })
         | RIGHT -> (n - 1, { dirs with right = right + 1 })) *)
(* | UP -> aux dir (n - 1, (u + 1, d, l, r))
   | DOWN -> (n - 1, (u, d + 1, l, r))
   | LEFT -> (n - 1, (u, d, l + 1, r))
   | RIGHT -> (n - 1, (u, d, l, r + 1))) *)
(* in
   let t = num_tiles - 1 ins
   match
     Adj_rules.fold_dirs aux (t, { up = 0; down = 0; left = 0; right = 0 })
   with
   | _, dirs -> dirs *)

let init_tile_enablers t adj = Adj_rules.count_init_enablers t adj

(* let make_test (l : int) =
   { collapsed = true; options = Array.make l 1.; sum_of_ones = 1 } *)

let make_grid x y c =
  let give_coords y x = c (x, y) in
  let make_row x y = Array.init x (give_coords y) in
  Array.init y (make_row x)

let make (x : int) (y : int) (t : int) (ws : float array) (adj : Adj_rules.t) =
  let sw, swlw = init_weight_sums ws in
  let enablers = init_tile_enablers t adj in
  let grid = make_grid x y (Cell.make (t + 1) sw swlw enablers) in
  (* let grid = make_grid x y (Cell.make t sw swlw enablers) in *)
  let heap = Pairing_heap.create ~min_size:(x * y) ~cmp:Cell.cmp () in
  let stack = Stack.create () in
  Array.iter (Array.iter (Pairing_heap.add heap)) grid;
  (* print_endline (Cell.enablers_to_string grid.(0).(0)); *)
  { grid; heap; stack; w = x; h = y; uncollapsed = x * y }

let make_test (x : int) (y : int) (tiles : Tile.t array) =
  {
    grid = Array.make_matrix x y (Cell.make_test tiles);
    heap = Pairing_heap.create ~min_size:0 ~cmp:(fun a b -> 0) ();
    stack = Stack.create ();
    w = x;
    h = y;
    uncollapsed = x * y;
  }

(* Array.make_matrix x y (Tile.make_test (Array.length cells)) *)

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
  List.iter (fun x -> Stack.push (c, x) st.stack) removed

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

let subtract_enablers ws t dir n st =
  if n.coords = (0, 0) then print_endline (Cell.enablers_to_string n);
  let opp_dir = Adj_rules.opposite_dir dir in
  let enablers = enablers_in_direction t opp_dir n in
  if enablers = 1 && not (Cell.has_zero_direction t n) then (
    Cell.remove_tile ws t n;
    Cell.check_contradiction n;
    Pairing_heap.add st.heap n;
    Stack.push (n, t) st.stack);
  decr_dir t opp_dir n

let rec propogate (ws : float array) (st : t) =
  try
    (* using pop allows us to exit early *)
    let c, tile = Stack.pop st.stack in
    (* print_endline ("cell:" ^ string_of_int tile); *)
    let neighbors = get_neighbors c st in
    let rec process_neighbors ns =
      match ns with
      | [] -> ()
      | (dir, (x, y)) :: t ->
          (* if st.grid.(y).(x).coords = (0, 0) then print_endline (Cell.enablers_to_string st.grid.(y).(x)); *)
          subtract_enablers ws tile dir st.grid.(y).(x) st;
          (* if st.grid.(x).(y).coords = (0, 0) then
             print_endline (Cell.enablers_to_string st.grid.(y).(x)); *)
          process_neighbors t
    in
    process_neighbors neighbors;
    propogate ws st
  with
  | Stack.Empty -> FINISHED st
  | Cell.Contradiction -> CONTRADICTION
(* let (i,j) = smallest_entropy st (Array.make (Array.length cells) 1.) in
   let tile = st.(i).(j) in *)

let draw (st : t) (x, y) (tiles : Tile.t array) =
  for i = 0 to Array.length st.grid - 1 do
    for j = 0 to Array.length st.grid.(0) - 1 do
      let index = st.grid.(i).(j).tile in
      let img = Tile.get_img tiles.(index) in
      let img_color_array = Graphics.dump_image img in
      let img_width = Array.length img_color_array in
      let img_height = Array.length img_color_array.(0) in
      Graphics.draw_image img (x + (img_width * i)) (y + (img_height * j))
    done
  done
