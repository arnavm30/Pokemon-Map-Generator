(* open modules *)
open Generator
open Graphics

(* draws a randomly sized and colored rectangle in the window *)
let rnd_rects () =
  Random.self_init ();
  let c = rgb (Random.int 255) (Random.int 255) (Random.int 255) in
  set_color c;
  fill_rect
    (Random.int (size_x ()))
    (Random.int (size_y ()))
    (Random.int (size_x ()))
    (Random.int (size_y ()))

(* [generate tiles] is the rendered testing map *)
let generate (tiles : Tile.t array) () =
  let width = size_x () in
  let height = size_y () in
  let map = State.make_test (width / 5) (height / 5) tiles in
  State.draw map 0 (height / 5) tiles

(* create the tiles with constraints filled in *)
let preload_tiles () : Tile.t array =
  (* edge constraints are given in clockwise order starting from up *)
  let init_tiles =
    [|
      Tile.make
        (Graphic_image.of_image (Png.load "assets/corner1.png" []))
        [| "green"; "green"; "grey"; "grey" |];
      Tile.make
        (Graphic_image.of_image (Png.load "assets/corner2.png" []))
        [| "green"; "grey"; "grey"; "green" |];
      Tile.make
        (Graphic_image.of_image (Png.load "assets/corner3.png" []))
        [| "grey"; "grey"; "green"; "green" |];
      Tile.make
        (Graphic_image.of_image (Png.load "assets/corner4.png" []))
        [| "grey"; "green"; "green"; "grey" |];
    |]
  in
  for i = 0 to Array.length init_tiles - 1 do
    let curr_cell = init_tiles.(i) in
    let mutated_cell = Tile.analyze curr_cell init_tiles in
    init_tiles.(i) <- mutated_cell
  done;
  init_tiles

(*--------------------------EVENT LOOP----------------------------------------*)

(* events associated with terminating application raise exception End*)
exception End

(* state of window *)
type map_state = { s : State.t; toggles : Toggle.t list; tiles : Tile.t array }

(* generate interface of UI elements, where to palce them *)
let gen_interface tiles width height (x, y) : Toggle.t list =
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
        radius img
    in
    r := tog :: !r
  done;
  !r

(* [skel f_init f_end f_mouse f_except] is the event loop *)
let skel f_init f_end f_key f_mouse f_except =
  f_init ();
  try
    while true do
      try
        let s = wait_next_event [ Button_down; Key_pressed ] in
        if s.keypressed then f_key s.key
        else if s.button then f_mouse s.mouse_x s.mouse_y
      with
      | End -> raise End
      | e -> f_except e
    done
  with End -> f_end ()

(* create the map state that'll be passed to functions in skel *)
let create_map_state () =
  open_graph "";
  resize_window 1450 800;
  let tiles = preload_tiles () in
  let width = size_x () in
  let height = size_y () in
  let toggles = gen_interface tiles width (height / 2) (0, 0) in
  { s = State.make_test (width / 5) (height / 5) tiles; toggles; tiles }

(* initialize things, render UI elements *)
let init map_st () = List.iter (fun t -> Toggle.draw t) map_st.toggles

(* handles what happens when mouse clicks *)
let f_mouse map_st x y =
  try
    let toggle = List.find (fun t -> Toggle.mem (x, y) t) map_st.toggles in
    let height = size_y () in
    Toggle.press toggle (fun b ->
        if b then State.draw map_st.s 0 (height / 5) map_st.tiles
        else clear_graph ())
  with Not_found -> print_endline "did not press toggle"

(* handles when program ends *)
let f_end e () = close_graph ()

(** [main ()] opens a graphics window and runs event loop*)
let main () =
  let s = create_map_state () in
  skel (init s) (f_end s) (fun _ -> ()) (f_mouse s) (fun _ -> ())

(* Execute the graphics engine. *)
let () = main ()
