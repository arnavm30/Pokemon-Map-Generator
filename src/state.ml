type t = bool array array

let make (x : int) (y : int) = Array.make_matrix x y true
let flip (x : int) (y : int) (st : t) = st.(x).(y) <- not st.(x).(y)
