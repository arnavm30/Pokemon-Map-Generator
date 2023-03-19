open Graphics

(** handle button presses*)
let rec button_press x =
  let status = wait_next_event x in
  let width = size_x () in
  let height = size_y () in
  if status.mouse_x >= width / 4 && status.mouse_x <= (width / 4) + (width / 2)
  then
    if
      status.mouse_y >= height / 20
      && status.mouse_y <= (height / 20) + (height / 10)
    then (
      set_color black;
      fill_rect (width / 4) (height / 5) (width / 2) (height / 10))
    else (
      print_endline "hello";
      button_press [ Button_down ])
  else (
    print_endline "hello2";
    button_press [ Button_down ])

(** [main ()] opens a graphics window*)
let main () =
  open_graph "";
  (* window closes as soon as script terminates, so wait until you press
     a key to keep window open*)
  set_color red;
  let width = size_x () in
  let height = size_y () in
  fill_rect (width / 4) (height / 20) (width / 2) (height / 10);
  button_press [ Button_down ];
  ignore (Graphics.read_key ())

(* Execute the graphics engine. *)
let () = main ()
