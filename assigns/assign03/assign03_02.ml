(* Forklist

   A `forklist` is combination of a list and a binary tree.  It has
   constructors for the empty list (Nil), a single link (Cons) and a
   double link (Fork).

   A forklist `l` with DISTINCT elements is ORDERED if it satisfies the
   following properties:

   1. If `l` is of the form `Cons (x, xs)` then every element in `xs` is
   greater than `x`.

   2. If `l` is of the form `Fork (x, lxs rxs)` then every element in
   `lxs` is less than x and every element in `rxs` is greater than
   `x`.

   A forklist `l` is TAILED if it satisfies the property that if `Cons
   (x, xs)` appears in `l`, then `xs` contains no `Fork`s.

   Implement a function `delay_cons` which given

     f : an ordered forklist of integers

   returns a TAILED ordered forklist with the following properties:

   1. It has the same elements as `f`

   2. It has the same number of `Cons`s, `Fork`s and `Nil`s as `f`.

   Example:
   let f = Cons (2, Fork(4, Cons(3, Nil), Cons (5, Nil))) in 
   let g = Fork (4, Cons (2, Cons (3, Nil)), Cons(5, Nil)) in 
   let _ = assert (delay_cons f = g)

   NOTE: the output does not need to look exactly like this. It just
   has to satisfy the above properties.

*)

type 'a forklist
  = Nil
  | Cons of 'a * 'a forklist
  | Fork of 'a * 'a forklist * 'a forklist

(*

let rec insert_in_tailed x = function
  | Nil -> Cons(x, Nil)  (* Inserting into Nil simply creates a Cons *)
  | Cons(y, ys) as cons -> if x < y then Cons(x, cons) else Cons(y, insert_in_tailed x ys)
  | Fork(y, lxs, rxs) -> if x < y then Fork(y, insert_in_tailed x lxs, rxs) else Fork(y, lxs, insert_in_tailed x rxs)

(* delay_cons implementation *)
let rec delay_cons (f : int forklist) : int forklist = match f with
  | Nil -> Nil
  | Cons(x, xs) -> insert_in_tailed x (delay_cons xs)  (* Insert x into the delayed version of xs *)
  | Fork(x, lxs, rxs) -> 
      let delayed_lxs = delay_cons lxs in
      let delayed_rxs = delay_cons rxs in
      insert_in_tailed x (Fork(x, delayed_lxs, delayed_rxs)) 

*)

let rec delay_cons (f : int forklist) : int forklist =
  match fl with
  | Nil | Cons (_, Nil) -> fl
  | Cons (x, Fork (y, l, r)) -> Fork (y, delay_cons (Cons (x, l)), delay_cons r)
  | Cons (x, y) -> Cons (x, delay_cons y)
  | Fork (x, l, r) -> Fork (x, delay_cons l, delay_cons r)