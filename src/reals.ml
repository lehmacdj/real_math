open Big_int (* Represents the index of a Real number approximation *)

(** Infix operators for big_ints *)
module Big_int_Infixes = struct
  let ( +- ) = add_big_int
  let ( +-- ) = add_int_big_int
  let ( -- ) = sub_big_int
  let ( *- ) = mult_big_int
  let ( *-- ) = mult_int_big_int
  let ( /- ) = div_big_int
  let ( =- ) = eq_big_int
  let ( <<- ) = lt_big_int
  let ( >- ) = gt_big_int
  let ( <=- ) = le_big_int
  let ( >=- ) = ge_big_int
  let ( <>- ) = fun a b -> not (eq_big_int a b)
end

open Big_int_Infixes

(** A module with some convenience functions on Natural numbers *)
module N = struct
  (* The additive and multiplicative identities *)
  let zero = zero_big_int
  let one = unit_big_int

  (* Functions *)
  let succ = ( +- ) one

  (* The numbers *)
  let two = succ one
  let three = succ two
end


open Num (* Represents rational part of Real sequence *)

(* This already includes infix operators *)

(** A module with some convenience functions on Rational numbers *)
module Q = struct
  (* The additive and multiplicative identities *)
  let zero = num_of_int 0
  let one = num_of_int 1

  (* Functions *)
  let succ = ( +/ ) one
  let inv x = one // x

  (* The numbers *)
  let two = succ one
  let three = succ two
end

(* Define some convenience operations on these two numeric types *)
let bi_of_n = big_int_of_num
let n_of_bi = num_of_big_int

module Real = struct
  type real = big_int -> num

  let zero_real = fun n -> Q.zero
  let unit_real = fun n -> Q.one

  (** the cannonical bound K_x as defined by bishop in chapter 2 *)
  let cannonical_bound (x: real): big_int =
    2 +-- abs_big_int (bi_of_n (ceiling_num (x N.one)))

  (** adds two real numbers according to bishops rules *)
  let add_real (x: real) (y: real): real = fun n ->
    let two_n = 2 *-- n in
    (x two_n) +/ (y two_n)

  (** multiply two real numbers according to bishops rules *)
  let mult_real (x: real) (y: real): real = fun n ->
    let k = max_big_int (cannonical_bound x) (cannonical_bound y) in
    let two_k_n = 2 *-- k *- n in
    (x two_k_n) */ (y two_k_n)

  (** the additive inverse of [x] *)
  let minus_real (x: real): real = fun n ->
    minus_num (x n)

  (** subtracts two real numbers according to bishops rules *)
  let sub_real (x: real) (y: real): real = add_real x (minus_real y)

  (** the multiplicative number of [x] *)
  let inverse_real (x: real): real = fun n ->
    let n' = bi_of_n (ceiling_num (Q.two */ ((x n) -/ (Q.inv (n_of_bi n))))) in
    if n' > n then Q.inv (x (n *- n *- n))
    else Q.inv (x (n *- n' *- n'))

  (** division of two real numbers according to bishops rules *)
  let div_real (x: real) (y: real): real = mult_real x (inverse_real y)

  (* Infix operators *)
  let ( ++ ) = add_real
  let ( -+ ) = sub_real
  let ( *+ ) = mult_real
  let ( /+ ) = div_real
  
  (** the default accuracy for functions that evaluate a real number *)
  let accuracy = ref 1000

  (** set the accuracy for functions that evaluate a real number to x *)
  let set_accuracy x = accuracy := x

  (** try looking at ps4 where I implemented a formatting function for strings
   * as a source of guidance for implementing stuffs *)


  module Math = struct
    let taylor_sum = failwith "unimplemented"
    let sqrt_real = failwith "unimplemented"
    let sin_real = failwith "unimplemented"
    let cos_real = failwith "unimplemented"
    let tan_real = failwith "unimplemented"
  end

end
