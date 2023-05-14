(* open modules *)
open Generator
open Graphics

type compn = Butn of Button.t | Tog of Toggle.t | LstPanel of List_panel.t
type size_data = { dims : int * int; place : int * int }

(* state of window *)
type map_state = {
  ui : compn list;
  tiles : Tile.t array;
  mutable chosen_tiles : Tile.t array;
  mutable size : string;
  size_data : size_data array;
}

(* based on panel, change size of map rendered *)
let change_size map_st (p : List_panel.t) () =
  map_st.size <- List_panel.get_active_text p

(* create the adjacency rules based on the given tiles*)
let create_adj_rules (tiles : Tile.t array) =
  let r = ref (Adj_rules.empty ()) in
  print_endline "initial adjacency rules, should be empty: ";
  Adj_rules.print_to_string !r;
  for i = 0 to Array.length tiles - 1 do
    let tile = tiles.(i) in
    let tile_up = Tile.get_up tile in
    let tile_right = Tile.get_right tile in
    let tile_down = Tile.get_down tile in
    let tile_left = Tile.get_left tile in
    let rules =
      Adj_rules.empty ()
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
  print_endline "reuslting adjacency rules: ";
  Adj_rules.print_to_string !r;
  !r

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
let create_map_state file_path () =
  open_graph "";
  resize_window 1450 800;
  let tiles, placements = Tile.from_json (Yojson.Basic.from_file file_path) in
  let width = size_x () in
  let height = size_y () in
  let components = gen_interface tiles width (height / 2) (0, 0) in
  {
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

(* clear renderings, redraw interface *)
let clear map_st () =
  clear_graph ();
  init map_st ()

(* run the wfc algorithm given the map state *)
let run_wfc map_st () =
  clear map_st ();
  choose_tiles map_st;
  let tiles_len = Array.length map_st.chosen_tiles in
  let adj_rules = create_adj_rules map_st.chosen_tiles in
  let map_size, map_posi =
    match map_st.size with
    | "small" -> (map_st.size_data.(0).dims, map_st.size_data.(0).place)
    | "medium" -> (map_st.size_data.(1).dims, map_st.size_data.(1).place)
    | "large" -> (map_st.size_data.(2).dims, map_st.size_data.(2).place)
    | _ -> (map_st.size_data.(0).dims, map_st.size_data.(0).place)
  in

  let result_state =
    Wfc.wfc map_size tiles_len (Array.make tiles_len 1.) adj_rules
  in
  State.draw result_state map_posi map_st.chosen_tiles

(* run the wfc algorithm concurrently given map state and status of button *)
let concurrent_wfc b map_st () =
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
    | LstPanel l -> List_panel.press l (x, y) (change_size map_st l)
  with Not_found -> print_endline "did not press a component"

(* handles what happens when there's a key press *)
let f_key map_st k = if k = ' ' then clear map_st () else ()

(** [main ()] opens a graphics window and runs event loop*)
let main () =
  let s = create_map_state "data/corners.json" () in
  event_loop (init s) (f_key s) (f_mouse s)

(* Execute the graphics engine. *)
let () = main ()
