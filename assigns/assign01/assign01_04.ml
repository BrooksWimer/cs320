(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)


let rec find_cubes start current target =  
    if start**3.0 >= target then 
      0.0

    else if current**3.0 >= target then 
      find_cubes (start+.1.0) (start+.1.0) target

    else if (start**3.0 +. current**3.0) = target then 
      let rec_call = find_cubes start (current+.1.0) target in 
        rec_call +. 1.0

    else 
      find_cubes start (current+.1.0) target
      
let taxicab (n : int) : int =
  let x = find_cubes 1.0 1.0 (float_of_int n) in
    int_of_float x

