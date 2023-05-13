let sumf (flts : float array) = Array.fold_left (fun s i -> s +. i) 0. flts
let sum (ints : int array) = Array.fold_left (fun s i -> s + i) 0 ints
let op_aux (op : 'a -> 'a -> 'a) a b = Array.map2 (fun a b -> op a b) a b
let ( +++. ) (a : float array) (b : float array) = op_aux ( +. ) a b
let ( ---. ) (a : float array) (b : float array) = op_aux ( -. ) a b
let ( ***. ) (a : float array) (b : float array) = op_aux ( *. ) a b
let ( ///. ) (a : float array) (b : float array) = op_aux ( /. ) a b

let entropy (w : float array) =
  let s = sumf w in
  Float.log s -. (sumf (w ***. Array.map Float.log w) /. s)

let string_of_int_pair (x, y) =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let string_of_int_list lst =
  let rec aux lst =
    match lst with [] -> "" | h :: t -> string_of_int h ^ ", " ^ aux t
  in
  "[" ^ aux lst ^ "]"
