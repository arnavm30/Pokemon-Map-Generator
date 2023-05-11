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

let generate (tiles : Tile.t array) () =
  let width = size_x () in
  let height = size_y () in
  let map = State.make_test (width / 5) (height / 5) tiles in
  State.draw map 0 (height / 5) tiles

(** [main ()] opens a graphics window *)
let main () =
  open_graph "";
  resize_window 1450 800;
  let tiles = preload_tiles () in
  let width = size_x () in
  let height = size_y () in
  let b =
    Button.make (width / 4) (height / 25) (width / 2) (height / 15) red
      "Generate"
  in
  let t = Toggle.make 0 (height / 10) width (height / 4) tiles in
  Toggle.press t (generate tiles);
  Button.press b (generate tiles)

(* Execute the graphics engine. *)
let () = main ()
