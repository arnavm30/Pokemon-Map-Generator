(* open Graphics *)
open Generator

(** [main ()] opens a graphics window*)
let main () =
  let map = State.make_test 37 28 2 in
  State.draw map 2 1

(* Execute the graphics engine. *)
let () = main ()
