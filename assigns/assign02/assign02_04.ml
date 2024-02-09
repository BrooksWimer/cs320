(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

type temp
  = Hot of int
  | Icy of int

let reduce (l : temp list) : temp list =
   let rec aux acc reduction_occurred = function
    | Hot i :: Icy j :: xs when i = j -> aux acc true xs
    | Icy i :: Hot j :: xs when i = j -> aux acc true xs
    | x :: xs -> aux (x :: acc) reduction_occurred xs
    | [] -> (List.rev acc, reduction_occurred)
  in
  let rec reduce_aux lst =
    let (reduced_list, reduction_occurred) = aux [] false lst in
    if reduction_occurred then reduce_aux reduced_list
    else reduced_list
  in
  reduce_aux l
