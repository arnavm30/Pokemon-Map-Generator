let op_aux (op : 'a -> 'a -> 'a) a b = Array.map2 (fun a b -> op a b) a b
let ( +++./ ) (a : float array) (b : float array) = op_aux ( +. ) a b
let ( ---./ ) (a : float array) (b : float array) = op_aux ( -. ) a b
let ( ***. ) (a : float array) (b : float array) = op_aux ( *. ) a b
let ( ///. ) (a : float array) (b : float array) = op_aux ( /. ) a b
let sum (flts : float array) = Array.fold_left (fun s i -> s +. i) 0. flts

let entropy (w : float array) =
  let s = sum w in
  Float.log s -. (sum (w ***. Array.map Float.log w) /. s)
