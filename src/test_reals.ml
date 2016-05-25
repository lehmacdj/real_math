open Reals
open Assertions
open Num
open Big_int
open Real

let two: real = real_of_num (num_of_int 2)

exception Reals_not_equal of string

let test_real x y =
  let x = string_of_real x in
  let y = string_of_real y in
  if x = y then () else raise (Reals_not_equal (x ^ " /= " ^ y))

(* Simple arithmatic *)
TEST_UNIT "1 + 1" = test_real unit_real unit_real
TEST_UNIT "1 * 3" = failwith "failure"
TEST_UNIT "2 * 2" = failwith "failure"
TEST_UNIT "6 * 6" = failwith "failure"

let () = Pa_ounit_lib.Runtime.summarize ()
