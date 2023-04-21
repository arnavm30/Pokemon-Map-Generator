open Graphics
open Generator

(** [main ()] opens a graphics window*)
let main () =
  open_graph "";
  resize_window 1000 1000;
  let img = Png.load "bin/big_tile copy.png" [] in
  let g = Graphic_image.of_image img in
  Graphics.draw_image g 0 0;
  (* window closes as soon as script terminates, so wait until you press
     a key to keep window open*)
  let width = size_x () in
  let height = size_y () in
  let b =
    Button.make (width / 4) (height / 20) (width / 2) (height / 10) red
      "Generate"
  in
  Button.press b;
  ignore (read_key ())

(* Execute the graphics engine. *)
let () = main ()
