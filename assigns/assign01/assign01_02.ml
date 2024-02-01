(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

 let rec divisors x y =
  if y = 1 then
    1
  else if (x mod y) = 0 && x <> y then
    let rec_call = divisors x (y-1) in 
      rec_call + y
  else 
    divisors x (y-1)


let is_perfect (n : int) : bool =
  let sum_divs = divisors n n in 
    sum_divs = n
  
