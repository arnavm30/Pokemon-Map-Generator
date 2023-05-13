(* open modules *)
open Generator
open Graphics

type compn = Butn of Button.t | Tog of Toggle.t

(* state of window *)
type map_state = {
  s : State.t;
  ui : compn list;
  tiles : Tile.t array;
  mutable chosen_tiles : Tile.t array;
}

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
        | Butn b -> acc)
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

let run_wfc map_st () =
  let tiles_len = Array.length map_st.tiles in
  choose_tiles map_st;
  let adj_rules = create_adj_rules map_st.chosen_tiles in
  let result_state =
    Wfc.wfc 20 20 tiles_len (Array.make tiles_len 1.) adj_rules
  in
  State.draw result_state 600 (size_y () / 2) map_st.chosen_tiles

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
  !r

(* [event_loop f_init f_key f_mouse] is the event loop *)
let event_loop f_init f_key f_mouse =
  f_init ();
  while true do
    let s = wait_next_event [ Button_down; Key_pressed ] in
    if s.keypressed then f_key s.key
    else if s.button then f_mouse s.mouse_x s.mouse_y
  done

(* create the map state that'll be passed to functions in event_loop *)
let create_map_state () =
  open_graph "";
  resize_window 1450 800;
  let tiles = Tile.from_json (Yojson.Basic.from_file "data/corners.json") in
  let width = size_x () in
  let height = size_y () in
  let components = gen_interface tiles width (height / 2) (0, 0) in
  {
    s = State.make_test (width / 5) (height / 5) tiles;
    ui = components;
    tiles;
    chosen_tiles = tiles;
  }

(* initialize things, render UI elements *)
let init map_st () =
  List.iter
    (fun c -> match c with Tog t -> Toggle.draw t | Butn b -> Button.draw b)
    map_st.ui

(* handles what happens when mouse clicks *)
let f_mouse map_st x y =
  try
    let compn =
      List.find
        (fun c ->
          match c with
          | Tog t -> Toggle.mem (x, y) t
          | Butn b -> Button.mem (x, y) b)
        map_st.ui
    in
    (* let height = size_y () in *)
    match compn with
    | Tog t ->
        Toggle.press t (fun b -> ())
        (* (fun b ->
            if b then State.draw map_st.s 0 (height / 5) map_st.tiles
            else clear_graph ()) *)
    | Butn b -> Button.press b (run_wfc map_st)
  with Not_found -> print_endline "did not press a component"

(** [main ()] opens a graphics window and runs event loop*)
let main () =
  let s = create_map_state () in
  event_loop (init s) (fun _ -> ()) (f_mouse s)

(* Execute the graphics engine. *)
let () = main ()
