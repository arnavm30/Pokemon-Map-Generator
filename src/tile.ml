type t = {
  img : Graphics.image;
  edges : string array;
  mutable up : int list;
  mutable right : int list;
  mutable down : int list;
  mutable left : int list;
}

let make (img : Graphics.image) (edges : string array) : t =
  { img; edges; up = []; right = []; down = []; left = [] }

let get_img (tile : t) = tile.img

let analyze (curr_tile : t) (tiles : t array) : t =
  for i = 0 to Array.length tiles - 1 do
    (* check bottom edge of cells.(i) matches top edge of curr_cell *)
    if tiles.(i).edges.(2) = curr_tile.edges.(0) then
      curr_tile.up <- i :: curr_tile.up
    else if tiles.(i).edges.(3) = curr_tile.edges.(1) then
      curr_tile.right <- i :: curr_tile.right
    else if tiles.(i).edges.(0) = curr_tile.edges.(2) then
      curr_tile.down <- i :: curr_tile.down
    else if tiles.(i).edges.(1) = curr_tile.edges.(3) then
      curr_tile.left <- i :: curr_tile.left
    else ()
  done;
  curr_tile
