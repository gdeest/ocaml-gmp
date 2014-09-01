open Gmp
open OUnit2

let test_of_int _ =
  let lst = [ (1, "1"); (-328, "-328"); (4096, "4096") ] in
  List.iter (fun (a, b) -> assert_equal Z.(to_string @@ of_int a) b) lst

let test_div_by_zero _ =
  let v1 = Z.of_int 3 in
  let f () = 
    ignore Z.(v1 / zero); () in
  assert_raises Division_by_zero f  


let suite =
  "Test Suite">:::[
    "of_int">:: test_of_int;
    "div_by_zero">:: test_div_by_zero
  ]

let _ =
  run_test_tt_main suite
