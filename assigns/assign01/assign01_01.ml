(* Reversing strings

   Please implement the function `string_rev` of type `string ->
   string` which, given a string `s`, returns the string with the same
   characters but in reverse order.

   Hint: Recall that there are no built-in functions for converting
   from `string` to `char or vice versa. See OCP 2.3.1 for details on
   how to accomplish this.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (string_rev "testing" = "gnitset")
   let _ = assert (string_rev "12345" = "54321")
   let _ = assert (string_rev "noon" = "noon")

 *)

let rec string_rev (s : string) : string =
  if (String.length s) = 1 then
    s
  else 
    let subbed = String.sub s 1 (String.length s-1) in
    let reved = string_rev(subbed) in 
    let first = s.[0] in 
    reved ^ (String.make 1 first)

