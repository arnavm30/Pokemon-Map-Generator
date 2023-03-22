open Graphics
open Generator

(** [main ()] opens a graphics window*)
let main () =
  open_graph "";
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
