open Graphics

type t = {
  x : int;
  y : int;
  width : int;
  height : int;
  c : color;
  text : string;
}

let make x y w h c str = { x; y; width = w; height = h; c; text = str }

let draw b =
  set_color b.c;
  let x = b.x in
  let y = b.y in
  let w = b.width in
  let h = b.height in
  fill_rect x y w h;
  let text_x, text_y = text_size b.text in
  moveto (x + (w / 3) + (text_x / 3)) (y + (text_y / 3));
  set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  set_color black;
  draw_string b.text

let mem (x, y) b =
  let x0 = b.x in
  let y0 = b.y in
  let w = b.width in
  let h = b.height in
  x >= x0 && x <= x0 + w && y >= y0 && y <= y0 + h

let press (b : t) (f : unit -> unit) : unit = f ()
