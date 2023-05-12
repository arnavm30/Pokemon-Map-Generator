open Graphics

type t = { x : int; y : int; r : int; img : Graphics.image; mutable on : bool }

let make (x : int) (y : int) (r : int) (img : Graphics.image) : t =
  { x; y; r; img; on = false }

let draw (toggle : t) : unit =
  set_color 0x757575;
  let img = toggle.img in
  let img_color_array = dump_image img in
  let img_width = Array.length img_color_array in
  let img_height = Array.length img_color_array.(0) in
  Graphics.draw_image img
    (toggle.x - (img_width / 2))
    (toggle.y + (2 * img_height));
  fill_circle toggle.x toggle.y toggle.r

let mem (x, y) (toggle : t) =
  let x0 = toggle.x in
  let y0 = toggle.y in
  let r = toggle.r in
  Float.hypot (float_of_int (x - x0)) (float_of_int (y - y0)) < float_of_int r

let press (toggle : t) (f : bool -> unit) : unit =
  if toggle.on then (
    set_color 0x757575;
    fill_circle toggle.x toggle.y toggle.r;
    toggle.on <- false)
  else (
    set_color green;
    fill_circle toggle.x toggle.y toggle.r;
    toggle.on <- true);
  f toggle.on
