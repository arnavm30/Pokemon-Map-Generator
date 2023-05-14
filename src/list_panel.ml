open Graphics

type e = { y0 : int; h0 : int; text : string; index : int }

type t = {
  x : int;
  y : int;
  width : int;
  height : int;
  options : e array;
  mutable active : int;
}

let make x y w h strs active =
  let num_strs = List.length strs in
  let strs_arr = Array.of_list strs in
  let r = Array.make num_strs { y0 = 0; h0 = 0; text = ""; index = active } in
  for i = 0 to num_strs - 1 do
    r.(i) <-
      {
        y0 = y + (i * h / num_strs);
        h0 = h / num_strs;
        text = strs_arr.(i);
        index = i;
      }
  done;
  { x; y; width = w; height = h; options = r; active }

let get_active_text p = p.options.(p.active).text

let draw p =
  set_color 0x757575;
  let x = p.x in
  let y = p.y in
  let w = p.width in
  let h = p.height in
  fill_rect x y w h;
  set_color 0x303030;
  let active_option = p.options.(p.active) in

  fill_rect x active_option.y0 w active_option.h0;
  set_color white;
  set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
  for i = 0 to Array.length p.options - 1 do
    moveto (x + (w / 3)) (p.options.(i).y0 + 5);
    draw_string p.options.(i).text
  done

let mem_helper (x, y) (x0, y0, w, h) =
  x >= x0 && x <= x0 + w && y >= y0 && y <= y0 + h

let mem (x, y) p =
  let x0 = p.x in
  let y0 = p.y in
  let w = p.width in
  let h = p.height in
  mem_helper (x, y) (x0, y0, w, h)

let press p (x, y) (f : unit -> unit) : unit =
  let selected_opt =
    Array.find_opt
      (fun o -> mem_helper (x, y) (p.x, o.y0, p.width, o.h0))
      p.options
  in
  match selected_opt with
  | Some sel ->
      p.active <- sel.index;
      draw p;
      f ()
  | None -> ()
