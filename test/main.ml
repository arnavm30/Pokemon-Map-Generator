open OUnit2
open Generator
open Util

(***************************************************************************)
(* Helper functions *)
(***************************************************************************)

let array_op_test (name : string) (arr1 : float array) (arr2 : float array)
    (op : float array -> float array -> float array)
    (expected_output : float array) : test =
  name >:: fun _ -> assert_equal expected_output (op arr1 arr2)

(***************************************************************************)
(* Test suite *)
(***************************************************************************)

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

let tests =
  [
    array_add_test "array add 0s array is same array" (Array.make 10 5.)
      (Array.make 10 0.) (Array.make 10 5.);
    array_sub_test "array sub 0s array is same array" (Array.make 10 5.)
      (Array.make 10 0.) (Array.make 10 5.);
    array_mul_test "array mul 1s array is same array" (Array.make 10 5.)
      (Array.make 10 1.) (Array.make 10 5.);
    array_div_test "array div 1s array is same array" (Array.make 10 5.)
      (Array.make 10 1.) (Array.make 10 5.);
  ]

let test_suite = "generator test suite" >::: tests
let _ = run_test_tt_main test_suite
