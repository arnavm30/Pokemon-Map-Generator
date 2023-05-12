(* open modules *)
open Generator
open Graphics

(*--------------------------EVENT LOOP----------------------------------------*)

type compn = Butn of Button.t | Tog of Toggle.t

(* state of window *)
type map_state = { s : State.t; ui : compn list; tiles : Tile.t array }

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
        radius img
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
  { s = State.make_test (width / 5) (height / 5) tiles; ui = components; tiles }

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
    let height = size_y () in
    match compn with
    | Tog t ->
        Toggle.press t (fun b ->
            if b then State.draw map_st.s 0 (height / 5) map_st.tiles
            else clear_graph ())
    | Butn b -> Button.press b (fun () -> clear_graph ())
  with Not_found -> print_endline "did not press a component"

(** [main ()] opens a graphics window and runs event loop*)
let main () =
  let s = create_map_state () in
  event_loop (init s) (fun _ -> ()) (f_mouse s)

(* Execute the graphics engine. *)
let () = main ()
