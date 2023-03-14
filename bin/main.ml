open Graphics

(** [main ()] opens a graphics window*)
let main () =
  open_graph "";
  print_endline "";
  print_int (size_y ());
  print_endline "";
  print_int (size_x ());
  (* window closes as soon as script terminates, so wait until you press
     a key to keep window open*)
  ignore (Graphics.read_key ())

(* Execute the graphics engine. *)
let () = main ()
