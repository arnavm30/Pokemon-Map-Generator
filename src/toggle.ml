open Graphics

type toggle = { mutable x : int; mutable y : int; r : int; mutable on : bool }

type t = {
  x : int;
  y : int;
  width : int;
  height : int;
  cells : Cells.t array;
  toggles : toggle array;
}

let make (x : int) (y : int) (w : int) (h : int) (cells : Cells.t array) : t =
  let cells_len = Array.length cells in
  let radius = 10 in
  let toggle_width = w / cells_len in
  let toggle_height = y + (h / 4) in
  let toggles =
    Array.make cells_len { x = 0; y = toggle_height; r = radius; on = false }
  in
  set_color red;
  for i = 0 to cells_len - 1 do
    let img = Cells.get_img cells.(i) in
    let img_color_array = dump_image img in
    let img_width = Array.length img_color_array in
    Graphics.draw_image img
      (x + (i * toggle_width) + (toggle_width / 2) - (img_width / 2))
      (toggle_height + (toggle_height / 2));
    toggles.(i).x <- x + (i * toggle_width) + (toggle_width / 2);
    fill_circle toggles.(i).x toggles.(i).y radius
  done;
  { x; y; width = w; height = h; cells; toggles }

let rec one_toggle_press (tggls : t) (toggle : toggle) (sts : status)
    (f : unit -> unit) : unit =
  if
    Float.hypot
      (float_of_int (sts.mouse_x - toggle.x))
      (float_of_int (sts.mouse_y - toggle.y))
    < float_of_int toggle.r
  then (
    set_color green;
    fill_circle toggle.x toggle.y toggle.r;
    f ();
    press tggls f)
  else (
    print_endline "did not press toggle";
    press tggls f)

and press (tggls : t) (f : unit -> unit) : unit =
  let status = wait_next_event [ Button_down ] in
  let toggles = tggls.toggles in
  for i = 0 to Array.length toggles - 1 do
    let toggle = toggles.(i) in
    one_toggle_press tggls toggle status f
  done;
  press tggls f
