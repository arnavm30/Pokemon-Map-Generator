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

(* create the cells with constraints filled in *)
let preload_cells () : Cells.t array =
  (* edge constraints are given in clockwise order starting from up *)
  let init_cells =
    [|
      Cells.make
        (Graphic_image.of_image (Png.load "assets/corner1.png" []))
        [| "green"; "green"; "grey"; "grey" |];
      Cells.make
        (Graphic_image.of_image (Png.load "assets/corner2.png" []))
        [| "green"; "grey"; "grey"; "green" |];
      Cells.make
        (Graphic_image.of_image (Png.load "assets/corner3.png" []))
        [| "grey"; "grey"; "green"; "green" |];
      Cells.make
        (Graphic_image.of_image (Png.load "assets/corner4.png" []))
        [| "grey"; "green"; "green"; "grey" |];
    |]
  in
  for i = 0 to Array.length init_cells - 1 do
    let curr_cell = init_cells.(i) in
    let mutated_cell = Cells.analyze curr_cell init_cells in
    init_cells.(i) <- mutated_cell
  done;
  init_cells

let generate (cells : Cells.t array) () =
  let width = size_x () in
  let height = size_y () in
  let map = State.make_test (width / 5) (height / 5) cells in
  State.draw map 0 (height / 5) cells

(** [main ()] opens a graphics window *)
let main () =
  open_graph "";
  resize_window 1450 800;
  let cells = preload_cells () in
  let width = size_x () in
  let height = size_y () in
  let b =
    Button.make (width / 4) (height / 25) (width / 2) (height / 15) red
      "Generate"
  in
  let t = Toggle.make 0 (height / 10) width (height / 4) cells in
  Toggle.press t (generate cells);
  Button.press b (generate cells)

(* Execute the graphics engine. *)
let () = main ()
