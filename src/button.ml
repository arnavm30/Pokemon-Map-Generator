open Graphics
open Random

type t = { x : int; y : int; width : int; height : int }

let make x y w h c str =
  set_color c;
  fill_rect x y w h;
  moveto (x + (w / 10)) (y + (y / 10));
  set_color black;
  set_text_size w;
  draw_string str;
  { x; y; width = w; height = h }

let rec press b =
  let status = wait_next_event [ Button_down ] in
  if
    status.mouse_x >= b.x
    && status.mouse_x <= b.x + b.width
    && status.mouse_y >= b.y
    && status.mouse_y <= b.y + b.height
  then (
    self_init ();
    let c = rgb (int 255) (int 255) (int 255) in
    set_color c;
    fill_rect
      (int (size_x ()))
      (int (size_y ()))
      (int (size_x ()))
      (int (size_y ()));
    press b)
  else (
    print_endline "did not press button";
    press b)
