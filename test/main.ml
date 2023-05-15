open OUnit2
open Generator
open Util

(***************************************************************************)
(* Helper functions *)
(***************************************************************************)

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_adjacency_rule rule =
  match rule with
  | i, j, dir ->
      "(" ^ string_of_int i ^ ", " ^ string_of_int j ^ ", "
      ^ Adj_rules.string_of_dir dir
      ^ ")"

let array_op_test (name : string) (arr1 : float array) (arr2 : float array)
    (op : float array -> float array -> float array)
    (expected_output : float array) : test =
  name >:: fun _ -> assert_equal expected_output (op arr1 arr2)

(***************************************************************************)
(* Test suite *)
(***************************************************************************)

(*------------------------Adj_rules Tests-------------------------------------*)
let allow_test (name : string) (index1 : int) (index2 : int)
    (dir : Adj_rules.directions) (rules : Adj_rules.t)
    (expected_output : (int * int * Adj_rules.directions) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Adj_rules.get_all_rules (Adj_rules.allow index1 index2 dir rules))
    ~printer:(pp_list pp_adjacency_rule)

let combine_test (name : string) (rules_1 : Adj_rules.t) (rules_2 : Adj_rules.t)
    (expected_output : (int * int * Adj_rules.directions) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Adj_rules.get_all_rules (Adj_rules.combine rules_1 rules_2))
    ~printer:(pp_list pp_adjacency_rule)

let is_allowed_test (name : string) (index1 : int) (index2 : int)
    (dir : Adj_rules.directions) (rules : Adj_rules.t) (expected_output : bool)
    : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Adj_rules.is_allowed index1 index2 dir rules)
    ~printer:string_of_bool

(* val count_init_enablers : int -> t -> Cell.directions array *)
(** [count_init_enablers t s] *)

let opposite_dir_test (name : string) (dir : Adj_rules.directions)
    (expected_output : Adj_rules.directions) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Adj_rules.opposite_dir dir)
    ~printer:Adj_rules.string_of_dir

(*------------------------Button Tests----------------------------------------*)
let button_mem_test (name : string) ((x, y) : int * int) (b : Button.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Button.mem (x, y) b) ~printer:string_of_bool

(*------------------------Cell Tests------------------------------------------*)
(*------------------------State Tests-----------------------------------------*)
(*------------------------Tile Tests------------------------------------------*)
(*------------------------Toggle Tests----------------------------------------*)

let test_img =
  Graphics.open_graph "";
  Graphics.make_image (Array.make_matrix 2 2 1)

let toggle_is_on_test (name : string) (t : Toggle.t) (expected_output : bool) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (Toggle.is_on t) ~printer:string_of_bool

let toggle_get_index_test (name : string) (t : Toggle.t) (expected_output : int)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (Toggle.get_index t) ~printer:string_of_int

let toggle_mem_test (name : string) ((x, y) : int * int) (t : Toggle.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Toggle.mem (x, y) t) ~printer:string_of_bool

(*------------------------Utility Tests---------------------------------------*)
let array_add_test (name : string) (arr1 : float array) (arr2 : float array)
    (expected_output : float array) : test =
  array_op_test name arr1 arr2 ( +++. ) expected_output

let array_sub_test (name : string) (arr1 : float array) (arr2 : float array)
    (expected_output : float array) : test =
  array_op_test name arr1 arr2 ( ---. ) expected_output

let array_mul_test (name : string) (arr1 : float array) (arr2 : float array)
    (expected_output : float array) : test =
  array_op_test name arr1 arr2 ( ***. ) expected_output

let array_div_test (name : string) (arr1 : float array) (arr2 : float array)
    (expected_output : float array) : test =
  array_op_test name arr1 arr2 ( ///. ) expected_output

let sumf_test (name : string) (flts : float array) (expected_output : float) :
    test =
  name >:: fun _ -> assert_equal expected_output (sumf flts)

let sum_test (name : string) (ints : int array) (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (sum ints)

(*---------------------------WFC Tests----------------------------------------*)

let tests =
  [
    (* adj_rules tests *)
    allow_test "simple add ajacency rule to empty set" 0 5 Adj_rules.UP
      (Adj_rules.empty 2)
      [ (0, 5, Adj_rules.UP) ];
    allow_test "simple add ajacency rule to non-empty set" 0 4 Adj_rules.DOWN
      (Adj_rules.empty 2 |> Adj_rules.allow 0 5 Adj_rules.UP)
      [ (0, 4, Adj_rules.DOWN); (0, 5, Adj_rules.UP) ];
    allow_test "add duplicate ajacency rule to non-empty set" 0 5
      Adj_rules.RIGHT
      (Adj_rules.empty 2 |> Adj_rules.allow 0 5 Adj_rules.RIGHT)
      [ (0, 5, Adj_rules.RIGHT); (0, 5, Adj_rules.RIGHT) ];
    allow_test
      "add duplicate indices ajacency rule with different directions to \
       non-empty set"
      0 5 Adj_rules.RIGHT
      (Adj_rules.empty 2 |> Adj_rules.allow 0 5 Adj_rules.LEFT)
      [ (0, 5, Adj_rules.RIGHT); (0, 5, Adj_rules.LEFT) ];
    is_allowed_test "is allow in empty set" 0 3 Adj_rules.RIGHT
      (Adj_rules.empty 2) false;
    is_allowed_test "is allow (0,3,right) in set with only that rule" 0 3
      Adj_rules.RIGHT
      (Adj_rules.allow 0 3 Adj_rules.RIGHT (Adj_rules.empty 2))
      true;
    is_allowed_test "is allow (0,3,right) in set with (0, 3, left)" 0 3
      Adj_rules.RIGHT
      (Adj_rules.allow 0 3 Adj_rules.LEFT (Adj_rules.empty 2))
      false;
    is_allowed_test "is allow (0,3,right) in set with (3, 0, right)" 0 3
      Adj_rules.RIGHT
      (Adj_rules.allow 3 0 Adj_rules.RIGHT (Adj_rules.empty 2))
      false;
    opposite_dir_test "opposite direction of UP = DOWN" Adj_rules.UP
      Adj_rules.DOWN;
    opposite_dir_test "opposite direction of DOWN = UP" Adj_rules.UP
      Adj_rules.DOWN;
    opposite_dir_test "opposite direction of LEFT = RIGHT" Adj_rules.LEFT
      Adj_rules.RIGHT;
    opposite_dir_test "opposite direction of RIGHT = LEFT" Adj_rules.RIGHT
      Adj_rules.LEFT;
    (* button tests *)
    button_mem_test "w/i edges of button mem test" (8, 10)
      (Button.make 0 0 10 20 Graphics.red "")
      true;
    button_mem_test "outside edges of button mem test" (11, 20)
      (Button.make 0 0 10 20 Graphics.red "")
      false;
    button_mem_test "on edge of button mem test" (11, 0)
      (Button.make 0 0 11 20 Graphics.red "")
      true;
    button_mem_test "on upper right corner of button mem test" (11, 20)
      (Button.make 0 0 11 20 Graphics.red "")
      true;
    button_mem_test "on lower left corner of button mem test" (0, 0)
      (Button.make 0 0 11 20 Graphics.red "")
      true;
    button_mem_test "on upper left corner of button mem test" (0, 20)
      (Button.make 0 0 11 20 Graphics.red "")
      true;
    button_mem_test "on lower right corner of button mem test" (11, 0)
      (Button.make 0 0 11 20 Graphics.red "")
      true;
    (* cell tests *)
    (* state tests *)
    (* tile tests *)
    (* toggle tests *)
    toggle_is_on_test "on toggle is on" (Toggle.make 10 10 10 test_img 0) true;
    toggle_get_index_test "toggle w/ index 1 = 1"
      (Toggle.make 10 10 10 test_img 1)
      1;
    toggle_get_index_test "toggle w/ index -1 = -1"
      (Toggle.make 10 10 10 test_img (-1))
      (-1);
    toggle_mem_test "w/i toggle mem test" (11, 5)
      (Toggle.make 10 10 10 test_img 0)
      true;
    toggle_mem_test "outside toggle mem test" (110, 5)
      (Toggle.make 10 10 10 test_img 0)
      false;
    (* util tests *)
    array_add_test "array add 0s array is same array" (Array.make 10 5.)
      (Array.make 10 0.) (Array.make 10 5.);
    array_sub_test "array sub 0s array is same array" (Array.make 10 5.)
      (Array.make 10 0.) (Array.make 10 5.);
    array_mul_test "array mul 1s array is same array" (Array.make 10 5.)
      (Array.make 10 1.) (Array.make 10 5.);
    array_div_test "array div 1s array is same array" (Array.make 10 5.)
      (Array.make 10 1.) (Array.make 10 5.);
    sumf_test "[] has 0 sum" (Array.make 0 0.) 0.;
    sum_test "[] has 0 sum" (Array.make 0 0) 0 (* wfc tests *);
  ]

let test_suite = "generator test suite" >::: tests
let _ = run_test_tt_main test_suite
