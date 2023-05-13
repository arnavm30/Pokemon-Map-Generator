open Yojson.Basic.Util

type t = {
  img : Graphics.image;
  edges : string array;
  mutable up : int list;
  mutable right : int list;
  mutable down : int list;
  mutable left : int list;
}

let make img edges = { img; edges; up = []; right = []; down = []; left = [] }
let get_img tile = tile.img
let get_up tile = tile.up
let get_right tile = tile.right
let get_down tile = tile.down
let get_left tile = tile.left

let analyze curr_tile tiles =
  (* reset indices to enable re-analyze same tiles *)
  curr_tile.up <- [];
  curr_tile.down <- [];
  curr_tile.left <- [];
  curr_tile.right <- [];
  for i = 0 to Array.length tiles - 1 do
    (* check bottom edge of cells.(i) matches top edge of curr_cell *)
    if tiles.(i).edges.(2) = curr_tile.edges.(0) then
      curr_tile.up <- i :: curr_tile.up;
    if tiles.(i).edges.(3) = curr_tile.edges.(1) then
      curr_tile.right <- i :: curr_tile.right;
    if tiles.(i).edges.(0) = curr_tile.edges.(2) then
      curr_tile.down <- i :: curr_tile.down;
    if tiles.(i).edges.(1) = curr_tile.edges.(3) then
      curr_tile.left <- i :: curr_tile.left
  done;
  curr_tile

let edges_of_json j =
  let up_edge = j |> member "up" |> to_string in
  let right_edge = j |> member "right" |> to_string in
  let down_edge = j |> member "down" |> to_string in
  let left_edge = j |> member "left" |> to_string in
  [| up_edge; right_edge; down_edge; left_edge |]

let tile_of_json j =
  let img_path = j |> member "img_path" |> to_string in
  let edges = j |> member "edges" |> edges_of_json in
  make (Graphic_image.of_image (Png.load img_path [])) edges

let from_json j =
  let tiles_lst = j |> member "tiles" |> to_list |> List.map tile_of_json in
  Array.of_list tiles_lst
