(* open modules *)
open Generator
open Graphics

type compn = Butn of Button.t | Tog of Toggle.t | LstPanel of List_panel.t
type size_data = { dim_x : int; dim_y : int; x : int; y : int }

(* state of window *)
type map_state = {
  s : State.t;
  ui : compn list;
  tiles : Tile.t array;
  mutable chosen_tiles : Tile.t array;
  mutable size : string;
  size_data : size_data array;
}

let change_size map_st (p : List_panel.t) () =
  map_st.size <- List_panel.get_active_text p

let create_adj_rules (tiles : Tile.t array) =
  let r = ref Adj_rules.empty in
  for i = 0 to Array.length tiles - 1 do
    let tile = tiles.(i) in
    let tile_up = Tile.get_up tile in
    let tile_right = Tile.get_right tile in
    let tile_down = Tile.get_down tile in
    let tile_left = Tile.get_left tile in
    let rules =
      Adj_rules.empty
      |> List.fold_right
           (fun indx acc -> Adj_rules.allow i indx Adj_rules.UP acc)
           tile_up
      |> List.fold_right
           (fun indx acc -> Adj_rules.allow i indx Adj_rules.DOWN acc)
           tile_down
      |> List.fold_right
           (fun indx acc -> Adj_rules.allow i indx Adj_rules.LEFT acc)
           tile_left
      |> List.fold_right
           (fun indx acc -> Adj_rules.allow i indx Adj_rules.RIGHT acc)
           tile_right
    in
    r := Adj_rules.combine rules !r
  done;
  Adj_rules.print_to_string !r;
  !r

let choose_tiles map_st =
  let compn_lst = map_st.ui in
  let index_lst =
    List.fold_left
      (fun acc c ->
        match c with
        | Tog t -> if Toggle.is_on t then Toggle.get_index t :: acc else acc
        | Butn b -> acc
        | LstPanel l -> acc)
      [] compn_lst
  in
  let index_array = Array.of_list index_lst in
  let index_len = Array.length index_array in
  let new_tiles = Array.make index_len map_st.tiles.(0) in
  for i = 0 to index_len - 1 do
    let index = index_array.(i) in
    new_tiles.(i) <- map_st.tiles.(index)
  done;
  for i = 0 to index_len - 1 do
    let curr_tile = new_tiles.(i) in
    let mutated_tile = Tile.analyze curr_tile new_tiles in
    new_tiles.(i) <- mutated_tile
  done;
  map_st.chosen_tiles <- new_tiles

(* let run_wfc map_st () =
   let tiles_len = Array.length map_st.tiles in
   choose_tiles map_st;
   let adj_rules = create_adj_rules map_st.chosen_tiles in
   let result_state =
     Wfc.wfc 40 40 tiles_len (Array.make tiles_len 1.) adj_rules
   in
   State.draw result_state 600 (size_y () / 2) map_st.chosen_tiles *)

(* this was used for testing, feel free to delete*)
(* let run_wfc map_st () =
   let tiles_len = Array.length map_st.tiles in
   choose_tiles map_st;
   let adj_rules = create_adj_rules map_st.chosen_tiles in
   let init_state = Wfc.init 2 2 tiles_len (Array.make tiles_len 1.) adj_rules in
   let ws = Array.make tiles_len 1. in
   let _ = State.collapse_cell ws init_state.grid.(0).(1) init_state in
   let state =
     match State.propogate tiles_len ws adj_rules init_state with
     | FINISHED state ->
         print_endline (string_of_int state.grid.(0).(1).tile);
         print_endline (Cell.enablers_to_string state.grid.(0).(0));
         state
     | CONTRADICTION -> raise Not_found
   in
   print_endline (string_of_int state.grid.(0).(1).tile);
   print_endline (Cell.enablers_to_string state.grid.(0).(0)) *)

(* State.draw state 600 (size_y () / 2) map_st.chosen_tiles *)

(*--------------------------EVENT LOOP----------------------------------------*)

(* generate interface of UI elements, where to place them *)
let gen_interface tiles width height (x, y) : compn list =
  let num_toggles = Array.length tiles in
  let toggle_width = width / num_toggles in
  let toggle_height = y + (height / 4) in
  let radius = 10 in
  let r = ref [] in
  for i = 0 to num_toggles - 1 do
    let img = Tile.get_img tiles.(i) in
    let img_color_array = dump_image img in
    let img_width = Array.length img_color_array in
    let tog =
      Toggle.make
        (x + (i * toggle_width) + (toggle_width / 2) - (img_width / 2))
        (toggle_height + (toggle_height / 2))
        radius img i
    in
    r := Tog tog :: !r
  done;
  let width = size_x () in
  let genr_butn = Button.make (width / 4) 50 (width / 2) 50 red "generate" in
  r := Butn genr_butn :: !r;
  let lst_pnl =
    List_panel.make (width / 15) 50 (width / 10) 75
      [ "small"; "medium"; "large" ]
      0
  in
  r := LstPanel lst_pnl :: !r;
  let file_lst_pnl =
    List_panel.make
      ((12 * width / 15) + 20)
      50 (width / 10) 75 [ "pipes"; "pokemon" ] 0
  in
  r := LstPanel file_lst_pnl :: !r;
  !r

(* [event_loop f_init f_key f_mouse] is the event loop *)
let event_loop f_init f_key f_mouse =
  f_init ();
  while true do
    let s = wait_next_event [ Button_down; Key_pressed ] in
    if s.keypressed then f_key s.key
    else if s.button then f_mouse s.mouse_x s.mouse_y
  done

let extract_size_data placements =
  let result =
    List.map
      (fun (size, size_data) ->
        match size_data with
        | [ ("dim_x", dim_x); ("dim_y", dim_y); ("x", x); ("y", y) ] ->
            { dim_x; dim_y; x; y }
        | _ -> failwith "something's wrong")
      placements
  in
  Array.of_list result

(* create the map state that'll be passed to functions in event_loop *)
let create_map_state () =
  open_graph "";
  resize_window 1450 800;
  (* let tiles = Tile.from_json (Yojson.Basic.from_file "data/corners.json") *)
  (* let tiles =
     Tile.from_json (Yojson.Basic.from_file "data/flexible_corners.json") *)
  let tiles, placements =
    Tile.from_json (Yojson.Basic.from_file "data/corners.json")
  in
  let width = size_x () in
  let height = size_y () in
  let components = gen_interface tiles width (height / 2) (0, 0) in
  {
    s = State.make_test (width / 5) (height / 5) tiles;
    ui = components;
    tiles;
    chosen_tiles = tiles;
    size = "small";
    size_data = extract_size_data placements;
  }

(* initialize things, render UI elements *)
let init map_st () =
  List.iter
    (fun c ->
      match c with
      | Tog t -> Toggle.draw t
      | Butn b -> Button.draw b
      | LstPanel l -> List_panel.draw l)
    map_st.ui

let clear map_st () =
  clear_graph ();
  init map_st ()

let run_wfc map_st () =
  clear map_st ();
  choose_tiles map_st;
  let tiles_len = Array.length map_st.chosen_tiles in
  let adj_rules = create_adj_rules map_st.chosen_tiles in
  let map_size =
    match map_st.size with
    | "small" -> (map_st.size_data.(0).dim_x, map_st.size_data.(0).dim_y)
    | "medium" -> (map_st.size_data.(1).dim_x, map_st.size_data.(1).dim_y)
    | "large" -> (map_st.size_data.(2).dim_x, map_st.size_data.(2).dim_y)
    | _ -> (map_st.size_data.(0).dim_x, map_st.size_data.(0).dim_y)
  in
  let result_state =
    Wfc.wfc map_size tiles_len (Array.make tiles_len 1.) adj_rules
  in
  let map_posi =
    match map_st.size with
    | "small" -> (map_st.size_data.(0).x, map_st.size_data.(0).y)
    | "medium" -> (map_st.size_data.(1).x, map_st.size_data.(1).y)
    | "large" -> (map_st.size_data.(2).x, map_st.size_data.(2).y)
    | _ -> (map_st.size_data.(0).x, map_st.size_data.(0).y)
  in
  State.draw result_state map_posi map_st.chosen_tiles

(* handles what happens when mouse clicks *)
let f_mouse map_st x y =
  try
    let compn =
      List.find
        (fun c ->
          match c with
          | Tog t -> Toggle.mem (x, y) t
          | Butn b -> Button.mem (x, y) b
          | LstPanel l -> List_panel.mem (x, y) l)
        map_st.ui
    in
    match compn with
    | Tog t -> Toggle.press t (fun b -> ())
    | Butn b -> Button.press b (run_wfc map_st)
    | LstPanel l -> List_panel.press l (x, y) (change_size map_st l)
  with Not_found -> print_endline "did not press a component"

let f_key map_st k =
  if k = ' ' then (
    clear_graph ();
    init map_st ())
  else ()

(** [main ()] opens a graphics window and runs event loop*)
let main () =
  let s = create_map_state () in
  event_loop (init s) (f_key s) (f_mouse s)

(* Execute the graphics engine. *)
let () = main ()
