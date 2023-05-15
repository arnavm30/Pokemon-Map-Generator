(* open modules *)
open Generator
open Graphics

type compn = Butn of Button.t | Tog of Toggle.t | LstPanel of List_panel.t
type size_data = { dims : int * int; place : int * int }

(* state of window *)
type map_state = {
  mutable ui : compn list;
  tiles : Tile.t array array;
  size_data : size_data array array;
  mutable active_tiles : int;
  mutable chosen_tiles : Tile.t array;
  mutable size : string;
}

(* based on the status of toggles, choose which tiles will be used *)
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
  let new_tiles = Array.make index_len map_st.tiles.(map_st.active_tiles).(0) in
  for i = 0 to index_len - 1 do
    let index = index_array.(i) in
    new_tiles.(i) <- map_st.tiles.(map_st.active_tiles).(index)
  done;
  for i = 0 to index_len - 1 do
    let curr_tile = new_tiles.(i) in
    let mutated_tile = Tile.analyze curr_tile new_tiles in
    new_tiles.(i) <- mutated_tile
  done;
  map_st.chosen_tiles <- new_tiles

(*--------------------------EVENT LOOP----------------------------------------*)

(* generate interface of UI elements, where to place them *)
let gen_interface tiles width height (x, y) active_size active_file : compn list
    =
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
      active_size
  in
  r := LstPanel lst_pnl :: !r;
  let file_lst_pnl =
    List_panel.make
      ((12 * width / 15) + 20)
      50 (width / 7) 75
      [ "pipes"; "pokemon grass"; "pokemon concrete"; "pokemon water" ]
      active_file
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

(* helper function to store size data *)
let extract_size_data placements =
  let result =
    List.map
      (fun (size, size_data) ->
        match size_data with
        | [ ("dim_x", dim_x); ("dim_y", dim_y); ("x", x); ("y", y) ] ->
            { dims = (dim_x, dim_y); place = (x, y) }
        | _ -> failwith "something's wrong")
      placements
  in
  Array.of_list result

(* create the map state that'll be passed to functions in event_loop *)
let create_map_state (files : string array) () =
  open_graph "";
  resize_window 1450 800;
  let file_tiles, file_placements =
    Tile.from_json (Yojson.Basic.from_file files.(0))
  in
  let files_len = Array.length files in
  let tiles_arr = Array.make files_len file_tiles in
  let sizes_arr = Array.make files_len (extract_size_data file_placements) in

  for i = 1 to files_len - 1 do
    let file_tiles, file_placements =
      Tile.from_json (Yojson.Basic.from_file files.(i))
    in
    tiles_arr.(i) <- file_tiles;
    sizes_arr.(i) <- extract_size_data file_placements
  done;
  let width = size_x () in
  let height = size_y () in
  let components = gen_interface tiles_arr.(0) width (height / 2) (0, 0) 0 0 in
  {
    ui = components;
    tiles = tiles_arr;
    size_data = sizes_arr;
    active_tiles = 0;
    chosen_tiles = tiles_arr.(0);
    size = "small";
  }

let update_map_state map_st =
  let width = size_x () in
  let height = size_y () in
  let active_size =
    match map_st.size with
    | "small" -> 0
    | "medium" -> 1
    | "large" -> 2
    | _ -> failwith "something's wrong with update map state"
  in
  let components =
    gen_interface
      map_st.tiles.(map_st.active_tiles)
      width (height / 2) (0, 0) active_size map_st.active_tiles
  in
  map_st.ui <- components;
  map_st.chosen_tiles <- map_st.tiles.(map_st.active_tiles)

(* initialize things, render UI elements *)
let init map_st () =
  List.iter
    (fun c ->
      match c with
      | Tog t -> Toggle.draw t
      | Butn b -> Button.draw b
      | LstPanel l -> List_panel.draw l)
    map_st.ui

(* clear renderings, redraw interface *)
let clear map_st () =
  clear_graph ();
  init map_st ()

let handle_menus map_st (p : List_panel.t) () =
  match List_panel.get_active_text p with
  | "small" -> map_st.size <- "small"
  | "medium" -> map_st.size <- "medium"
  | "large" -> map_st.size <- "large"
  | "pipes" ->
      map_st.active_tiles <- 0;
      update_map_state map_st;
      clear map_st ()
  | "pokemon grass" ->
      map_st.active_tiles <- 1;
      update_map_state map_st;
      clear map_st ()
  | "pokemon concrete" ->
      map_st.active_tiles <- 2;
      update_map_state map_st;
      clear map_st ()
  | "pokemon water" ->
      map_st.active_tiles <- 3;
      update_map_state map_st;
      clear map_st ()
  | _ -> failwith "what's going on"

(* run the wfc algorithm given the map state *)
let run_wfc map_st () =
  clear map_st ();
  choose_tiles map_st;
  let tiles_len = Array.length map_st.chosen_tiles in
  let adj_rules = Tile.create_adj_rules map_st.chosen_tiles in
  let active_size_data = map_st.size_data.(map_st.active_tiles) in
  let map_size, map_posi =
    match map_st.size with
    | "small" -> (active_size_data.(0).dims, active_size_data.(0).place)
    | "medium" -> (active_size_data.(1).dims, active_size_data.(1).place)
    | "large" -> (active_size_data.(2).dims, active_size_data.(2).place)
    | _ -> (active_size_data.(0).dims, active_size_data.(0).place)
  in
  let result_state =
    Wfc.wfc map_size tiles_len (Array.make tiles_len 1.) adj_rules
  in
  (* used for testing *)
  State.validate adj_rules result_state;
  State.draw result_state map_posi map_st.chosen_tiles

let copy_map_state { ui; tiles; size_data; active_tiles; chosen_tiles; size } =
  let copy_tiles =
    Array.init (Array.length chosen_tiles) (fun i -> Tile.copy chosen_tiles.(i))
  in
  { ui; tiles; size_data; active_tiles; chosen_tiles = copy_tiles; size }

(* run the wfc algorithm concurrently given map state and status of button *)
let concurrent_wfc b map_st () =
  (* let map_st = copy_map_state map_st in *)
  let _ =
    Thread.create
      (fun () ->
        Button.disallow_press b;
        run_wfc map_st ();
        Button.allow_press b)
      ()
  in
  ()

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
    | Butn b -> Button.press b (concurrent_wfc b map_st)
    | LstPanel l -> List_panel.press l (x, y) (handle_menus map_st l)
  with Not_found -> print_endline "did not press a component"

(* handles what happens when there's a key press *)
let f_key map_st k = if k = ' ' then clear map_st () else ()

(** [main ()] opens a graphics window and runs event loop*)
let main () =
  let s =
    create_map_state
      (* "data/orners.json" *)
      (* "data/flexible_corners.json" *)
      [|
        "data/corners.json";
        "data/pokemon_grass.json";
        "data/pokemon_concrete.json";
        "data/pokemon_water.json";
      |]
      ()
  in
  event_loop (init s) (f_key s) (f_mouse s)

(* Execute the graphics engine. *)
let () = main ()
