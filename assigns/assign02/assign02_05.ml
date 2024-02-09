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
  if len = 0 then 
    if stp = endp then [[]] else [] (* Only an empty path can have 0 length and be valid *)
  else if len mod 2 = 1 then [] (* Odd lengths cannot return to origin or satisfy alternating constraint *)
  else
    let rec generate_paths acc depth last_dir =
      if depth = len then 
        if stp = endp then [List.rev acc] else []
      else 
        let possible_dirs = [N; S; E; W] |> List.filter ((<>) last_dir) in
        List.fold_left (fun acc dir ->
          let next_step = match dir with
            | N -> {stp with y = stp.y + 1}
            | S -> {stp with y = stp.y - 1}
            | E -> {stp with x = stp.x + 1}
            | W -> {stp with x = stp.x - 1}
          in
          if depth + 1 = len then
            if next_step = endp then acc @ [[(dir, 1)]]
            else acc
          else
            generate_paths ((dir, 1)::acc) (depth + 1) dir |> List.map (fun path -> (dir, 1) :: path)
        ) [] possible_dirs
    in
    generate_paths [] 0 None
