open Yojson.Basic.Util
module StrSet = Set.Make (String)

type t = {
  img : Graphics.image;
  edges : StrSet.t array;
  mutable up : int list;
  mutable right : int list;
  mutable down : int list;
  mutable left : int list;
}

let lst_to_set (lst : string list) : StrSet.t =
  List.fold_left (fun acc el -> StrSet.add el acc) StrSet.empty lst

let make img edges =
  let new_edges = Array.map (fun el -> lst_to_set el) edges in
  { img; edges = new_edges; up = []; right = []; down = []; left = [] }

let get_img tile = tile.img
let get_up tile = tile.up
let get_right tile = tile.right
let get_down tile = tile.down
let get_left tile = tile.left
let is_overlap s1 s2 = not (StrSet.is_empty (StrSet.inter s1 s2))

let analyze curr_tile tiles =
  (* reset indices to enable re-analyze same tiles *)
  curr_tile.up <- [];
  curr_tile.down <- [];
  curr_tile.left <- [];
  curr_tile.right <- [];
  for i = 0 to Array.length tiles - 1 do
    (* check bottom edge of cells.(i) matches top edge of curr_cell *)
    if is_overlap tiles.(i).edges.(2) curr_tile.edges.(0) then
      curr_tile.up <- i :: curr_tile.up;
    if is_overlap tiles.(i).edges.(3) curr_tile.edges.(1) then
      curr_tile.right <- i :: curr_tile.right;
    if is_overlap tiles.(i).edges.(0) curr_tile.edges.(2) then
      curr_tile.down <- i :: curr_tile.down;
    if is_overlap tiles.(i).edges.(1) curr_tile.edges.(3) then
      curr_tile.left <- i :: curr_tile.left
  done;
  curr_tile

let edges_of_json j =
  let up_edge = j |> member "up" |> to_list |> List.map to_string in
  let right_edge = j |> member "right" |> to_list |> List.map to_string in
  let down_edge = j |> member "down" |> to_list |> List.map to_string in
  let left_edge = j |> member "left" |> to_list |> List.map to_string in
  [| up_edge; right_edge; down_edge; left_edge |]

let tile_of_json j =
  let img_path = j |> member "img_path" |> to_string in
  let edges = j |> member "edges" |> edges_of_json in
  make (Graphic_image.of_image (Png.load img_path [])) edges

let sizes_of_json j =
  let dim_x = j |> member "dim_x" |> to_int in
  let dim_y = j |> member "dim_y" |> to_int in
  let x = j |> member "x" |> to_int in
  let y = j |> member "y" |> to_int in
  [ ("dim_x", dim_x); ("dim_y", dim_y); ("x", x); ("y", y) ]

let palcement_of_json j =
  let small = j |> member "small" |> sizes_of_json in
  let medium = j |> member "medium" |> sizes_of_json in
  let large = j |> member "large" |> sizes_of_json in
  [ ("small", small); ("medium", medium); ("large", large) ]

let from_json j =
  let tiles_lst = j |> member "tiles" |> to_list |> List.map tile_of_json in
  let placement = j |> member "placement" |> palcement_of_json in
  (Array.of_list tiles_lst, placement)

let copy { img; edges; up; down; left; right } =
  let copy_edges = Array.init (Array.length edges) (fun i -> edges.(i)) in
  { img; edges = copy_edges; up; down; left; right }
