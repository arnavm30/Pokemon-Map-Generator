open Graphics

type t = {
  x : int;
  y : int;
  width : int;
  height : int;
  c : color;
  text : string;
  mutable can_be_pressed : bool;
}

let make x y w h c str =
  { x; y; width = w; height = h; c; text = str; can_be_pressed = true }

let draw b =
  set_color b.c;
  let x = b.x in
  let y = b.y in
  let w = b.width in
  let h = b.height in
  fill_rect x y w h;
  let text_x, text_y = text_size b.text in
  moveto (x + (w / 3) + (text_x / 3) + 20) (y + (text_y / 3));
  set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  set_color black;
  draw_string b.text

let mem (x, y) b =
  let x0 = b.x in
  let y0 = b.y in
  let w = b.width in
  let h = b.height in
  x >= x0 && x <= x0 + w && y >= y0 && y <= y0 + h

let allow_press b = b.can_be_pressed <- true
let disallow_press b = b.can_be_pressed <- false

let press (b : t) (f : unit -> unit) : unit =
  if b.can_be_pressed then f () else ()
