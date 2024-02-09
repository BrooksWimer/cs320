(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

(* Helper function to add an element to the correct type of list in the accumulator *)
let add_to_list acc elem = match acc, elem with
  | IntList ints, Int i -> IntList (List.rev (i :: ints))
  | StringList strs, String s -> StringList (List.rev  (s :: strs))

(* The convert function *)
let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec aux acc current = function
    | [] -> List.rev (current :: acc) (* No more elements, add the current group to acc and reverse acc *)
    | x :: xs -> (
        match current, x with
        | IntList il, Int i -> aux acc (add_to_list current x) xs
        | StringList sl, String s -> aux acc (add_to_list current x) xs
        | _, Int i -> aux (current :: acc) (IntList [i]) xs
        | _, String s -> aux (current :: acc) (StringList [s]) xs
      )
  in
  match l with
  | [] -> []
  | Int i :: xs -> aux [] (IntList [i]) xs
  | String s :: xs -> aux [] (StringList [s]) xs

