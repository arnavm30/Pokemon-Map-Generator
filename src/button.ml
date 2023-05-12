open Graphics

type t = { x : int; y : int; width : int; height : int }

let make (x : int) (y : int) (w : int) (h : int) (c : color) (str : string) : t
    =
  set_color c;
  fill_rect x y w h;
  moveto (x + (w / 3)) (y + (y / 2));
  set_color black;
  draw_string str;
  { x; y; width = w; height = h }

let mem (x, y) (x0, y0, w, h) = x >= x0 && x <= x0 + w && y >= y0 && y <= y0 + h

let rec press (b : t) (f : unit -> unit) : unit =
  let status = wait_next_event [ Button_down ] in
  if
    status.mouse_x >= b.x
    && status.mouse_x <= b.x + b.width
    && status.mouse_y >= b.y
    && status.mouse_y <= b.y + b.height
  then (
    f ();
    press b f)
  else (
    print_endline "did not press button";
    press b f)
