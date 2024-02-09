(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)
(* Define the types for directions and steps. *)
type dir = N | S | E | W
type step = dir * int
type point = { x: int; y: int; }

(* The all_paths function generates all possible paths from stp to endp with exactly len steps. *)
let all_paths len (stp: point) (endp: point) : (dir * int) list list =
  (* Directly return an empty list for odd lengths when stp and endp are the same,
     since it's impossible to return to the origin in an odd number of steps
     while adhering to the rule against repeating directions consecutively. *)
  if len mod 2 = 1 && stp = endp then []
  else
    (* Defines a helper function to simulate moving in a direction from a given point. *)
    let move (p: point) (d: dir) : point =
      match d with
      | N -> {p with y = p.y + 1}
      | S -> {p with y = p.y - 1}
      | E -> {p with x = p.x + 1}
      | W -> {p with x = p.x - 1} in

    (* Recursive function to generate all paths of a given length.
       It concatenates every possible direction to each path from the previous step. *)
    let rec generate_paths len =
      if len = 0 then [[]]  (* Base case: no steps left to take. *)
      else
        let dirs = [N; S; E; W] in  (* All possible directions. *)
        List.concat_map (fun d ->
          (* For each direction, add it to the start of all paths generated for one less step,
             creating new paths that are one step longer. *)
          List.map (fun path -> (d, 1) :: path) (generate_paths (len - 1))
        ) dirs in

    (* Merges consecutive steps in the same direction into a single step with summed lengths.
       This post-processing step simplifies paths to match the expected output format. *)
    let rec merge_consecutive = function
      | (d1, n1) :: (d2, n2) :: rest when d1 = d2 -> merge_consecutive ((d1, n1 + n2) :: rest)
      | h :: t -> h :: merge_consecutive t
      | [] -> [] in

    (* Generate all paths, then merge consecutive steps, and finally filter
       to keep only those paths that precisely lead to the endp. *)
    let paths = generate_paths len in
    let merged_paths = List.map merge_consecutive paths in
    List.filter (fun path ->
      (* Calculate the final position after following a path, and keep the path if it ends at endp. *)
      let final_pos = List.fold_left (fun acc (d, n) -> move acc d) stp path in
      final_pos = endp
    ) merged_paths