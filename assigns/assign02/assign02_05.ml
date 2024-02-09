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

type dir = N | S | E | W
type point = { x : int; y : int; }

let rec all_paths len stp endp =
  let move (dir, steps) {x; y} = match dir with
    | N -> {x; y = y + steps}
    | S -> {x; y = y - steps}
    | E -> {x = x + steps; y}
    | W -> {x = x - steps; y} in
  
  let append_step path dir = 
    match path with
    | (d, n) :: t when d = dir -> (dir, n + 1) :: t
    | _ -> (dir, 1) :: path in
  
  let rec explore_paths len last_dir path stp =
    if len = 0 then
      if stp = endp then [List.rev path] else []
    else
      [N; S; E; W]
      |> List.filter ((<>) last_dir)
      |> List.concat_map (fun dir ->
           explore_paths (len - 1) (Some dir) (append_step path dir) (move (dir, 1) stp)) in
  
  if len = 0 then
    if stp = endp then [[]] else []
  else if len mod 2 = 1 && stp = endp then
    [] (* No valid paths for odd lengths when starting and ending points are the same *)
  else
    explore_paths len None [] stp
