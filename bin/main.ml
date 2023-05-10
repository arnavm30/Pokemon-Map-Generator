(* open modules *)
open Generator
open Graphics

(* create the cells with constraints filled in *)
let preload_cells () : Cells.t array =
  (* edge constraints are given in clockwise order starting from up *)
  open_graph "";
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

(** [main ()] opens a graphics window *)
let main () =
  let cells = preload_cells () in
  let map = State.make_test 37 28 cells in
  State.draw map 2 1 cells

(* Execute the graphics engine. *)
let () = main ()
