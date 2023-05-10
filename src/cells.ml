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

let get_img (cell : t) = cell.img

let analyze (curr_cell : t) (cells : t array) : t =
  for i = 0 to Array.length cells - 1 do
    (* check bottom edge of cells.(i) matches top edge of curr_cell *)
    if cells.(i).edges.(2) = curr_cell.edges.(0) then
      curr_cell.up <- i :: curr_cell.up
    else if cells.(i).edges.(3) = curr_cell.edges.(1) then
      curr_cell.right <- i :: curr_cell.right
    else if cells.(i).edges.(0) = curr_cell.edges.(2) then
      curr_cell.down <- i :: curr_cell.down
    else if cells.(i).edges.(1) = curr_cell.edges.(3) then
      curr_cell.left <- i :: curr_cell.left
    else ()
  done;
  curr_cell
