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

type int_list_or_string_list = IntList of int list | StringList of string list
type int_or_string = Int of int | String of string

let convert (l : int_or_string list) : int_list_or_string_list list =
  (* Auxiliary function to process the list. *)
  let rec aux acc current_ints current_strs = function
    | [] -> 
        (* Finalize the accumulation by handling any remaining ints or strings. *)
        let acc = match current_ints with
                  | [] -> acc
                  | _ -> IntList (List.rev current_ints) :: acc in
        let acc = match current_strs with
                  | [] -> acc
                  | _ -> StringList (List.rev current_strs) :: acc in
        List.rev acc
    | Int i :: xs ->
        (* When encountering an Int, add it to the current_ints and reset current_strs. *)
        if current_strs <> [] then
          aux (StringList (List.rev current_strs) :: acc) [i] [] xs
        else
          aux acc (i :: current_ints) [] xs
    | String s :: xs ->
        (* When encountering a String, add it to the current_strs and reset current_ints. *)
        if current_ints <> [] then
          aux (IntList (List.rev current_ints) :: acc) [] [s] xs
        else
          aux acc [] (s :: current_strs) xs
  in
  aux [] [] [] l

(* Example usage and assertion to validate the function's correctness. *)
let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
let () = assert (convert test_in = test_out)
