open Graphics

type t = {
  x : int;
  y : int;
  r : int;
  img : Graphics.image;
  tile_index : int;
  mutable on : bool;
}

let make x y r img index = { x; y; r; img; tile_index = index; on = true }
let is_on toggle = toggle.on
let get_index toggle = toggle.tile_index

let draw toggle =
  set_color green;
  let img = toggle.img in
  let img_color_array = dump_image img in
  let img_width = Array.length img_color_array in
  let img_height = Array.length img_color_array.(0) in
  Graphics.draw_image img
    (toggle.x - (img_width / 2))
    (toggle.y + (2 * img_height));
  fill_circle toggle.x toggle.y toggle.r

let mem (x, y) toggle =
  let x0 = toggle.x in
  let y0 = toggle.y in
  let r = toggle.r in
  Float.hypot (float_of_int (x - x0)) (float_of_int (y - y0)) < float_of_int r

let press toggle f =
  if toggle.on then (
    set_color 0x757575;
    fill_circle toggle.x toggle.y toggle.r;
    toggle.on <- false)
  else (
    set_color green;
    fill_circle toggle.x toggle.y toggle.r;
    toggle.on <- true);
  f toggle.on
