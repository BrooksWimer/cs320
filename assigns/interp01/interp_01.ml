(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident =
  ws >> many1 (satisfy is_upper_case) << ws >|= implode

let parse_num =
  ws >> many1 (satisfy is_digit) << ws >|= fun digits -> Num (int_of_string (implode digits))


(* simple commands *)
let parse_simple_command =
  choice [
    keyword "drop" >> pure Drop;
    keyword "swap" >> pure Swap;
    keyword "dup" >> pure Dup;
    keyword "." >> pure Trace;
    keyword "+" >> pure Add;
    keyword "-" >> pure Sub;
    keyword "*" >> pure Mul;
    keyword "/" >> pure Div;
    keyword "<" >> pure Lt;
    keyword "=" >> pure Eq;
    parse_num;
  ]



(* compound commands *)

let rec parse_com () =
  choice [
    parse_simple_command;
    parse_bind;
    parse_call;
    parse_if ();
    parse_def ();
    parse_plain_ident;
  ]

and parse_plain_ident = 
  parse_ident >|= fun id -> Ident id 

and parse_bind =
  (keyword "|>" >> parse_ident) >|= fun id -> Bind id

and parse_call =
  (keyword "#" >> parse_ident) >|= fun id -> Call id

and parse_if () =
  (keyword "?" >> parse_prog_rec () << keyword ";") >|= fun p -> If p

and parse_def () =
  map2 (fun id p -> Def (id, p))
       (keyword "def" >> parse_ident << ws)
       (parse_prog_rec () << keyword ";")

and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

let parse_prog = 
  many (ws >> parse_com () << ws)

(* A VERY SMALL TEST SET *)

let test = parse parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)


let test = parse parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)
(**)

(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list
type config = int list * env * trace * program

let show_value = function
  | Num i -> string_of_int i
  | Prog _ -> "<program>"

let show_env env =
  env
  |> List.map (fun (id, value) -> id ^ " -> " ^ show_value value)
  |> String.concat ", "

let show_stack stack =
  stack |> List.map string_of_int |> String.concat ", "

let show_trace trace =
  String.concat ", " trace
(* Assuming you have a function like this, which needs to be defined based on your `command` type *)
let show_command cmd =
  match cmd with
  | Drop -> "Drop"
  | Swap -> "Swap"
  | Dup -> "Dup"
  | Trace -> "Trace"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Lt -> "Lt"
  | Eq -> "Eq"
  | Bind id -> "|> " ^ id
  | Call id -> "# " ^ id
  | If _ -> "If <prog> ;" (* This needs to be handled depending on how you want to show nested programs *)
  | Def (id, _) -> "def " ^ id ^ " <prog> ;" (* Same here for showing the program part *)
  | Ident id -> id
  | Num n -> string_of_int n

(* Function to convert a list of commands (program) to a single string *)
let show_program prog =
  prog
  |> List.map show_command  (* Convert each command to its string representation *)
  |> String.concat ", "    (* Concatenate all command strings with a comma separator *)


let print_config (stack, env, trace, prog) =
  Printf.printf "Stack: [%s]\n" (show_stack stack);
  Printf.printf "Environment: {%s}\n" (show_env env);
  Printf.printf "Trace: [%s]\n" (show_trace trace);
  Printf.printf "Program: %s\n" (show_program prog);
  print_endline "---------------------------"


let rec fetch_env env x = 
  match env with
  | [] -> None  
  | (id, val_)::rest ->
      if id = x then Some val_
      else fetch_env rest x

let update_env env x v = 
  let rec update_aux env acc =
    match env with
    | [] -> (x, v) :: acc  (* If end of list reached without finding x, add new binding *)
    | (id, val_)::rest ->
        if id = x then (List.rev acc) @ ((x, v) :: rest)  (* Update found identifier *)
        else update_aux rest ((id, val_) :: acc)  (* Keep searching *)
  in 
  update_aux env []


let run_command (stk, env, trace, prog) (cmd : command) : config =
  match cmd, stk with
  | Num n, stk -> (n :: stk, env, trace, prog)
  | Drop, x :: rest -> (rest, env, trace, prog)
  | Drop, [] -> ([], env, "panic" :: trace, [])
  | Add, x :: y :: rest -> ((x + y) :: rest, env, trace, prog)
  | Add, _ -> (stk, env, "panic" :: trace, [])
  | Sub, x :: y :: rest -> ((x - y) :: rest, env, trace, prog)
  | Sub, _ -> (stk, env, "panic" :: trace, [])
  | Mul, x :: y :: rest -> ((x * y) :: rest, env, trace, prog)
  | Mul, _ -> (stk, env, "panic" :: trace, [])
  | Div, x :: 0 :: rest -> (stk, env, "panic" :: trace, [])
  | Div, x :: y :: rest -> ((x / y) :: rest, env, trace, prog)
  | Div, _ -> (stk, env, "panic" :: trace, [])
  | Dup, x :: rest -> (x :: x :: rest, env, trace, prog)
  | Dup, [] -> ([], env, "panic" :: trace, [])
  | Lt, x :: y :: rest -> ((if x < y then 1 else 0) :: rest, env, trace, prog)
  | Lt, _ -> (stk, env, "panic" :: trace, [])
  | Eq, x :: y :: rest -> ((if x = y then 1 else 0) :: rest, env, trace, prog)
  | Eq, _ -> (stk, env, "panic" :: trace, [])
  | Swap, x :: y :: rest -> (y :: x :: rest, env, trace, prog)
  | Swap, _ -> (stk, env, "panic" :: trace, [])
  | Trace, x :: rest -> (stk, env, (string_of_int x) :: trace, prog)
  | Trace, [] -> ([], env, "panic" :: trace, [])
  | Bind id, rest -> (match rest with x :: xs -> (xs, update_env env id (Num x), trace, prog) | [] -> (stk, env, "panic" :: trace, []))
  | Ident id, stk -> 
      (match fetch_env env id with
       | Some (Num v) -> (v :: stk, env, trace, prog)
       | _ -> (stk, env, "panic" :: trace, [])) 
  | Def (id, q), _ -> (stk, update_env env id (Prog q), trace, prog)
  | Call id, _ ->
      (match fetch_env env id with
       | Some (Prog p) -> (stk, env, trace, p @ prog)
       | _ -> (stk, env, "panic" :: trace, []))
  | If q, 0 :: rest -> (rest, env, trace, prog)
  | If q, _ :: rest -> (rest, env, trace, q @ prog)
  | If q, [] -> ([], env, "panic" :: trace, [])



(* Function to run a list of commands *)
let rec run_program config =
  match config with 
  | (stk, env, trace, commands) -> (
          match commands with
            | [] -> config
            | cmd :: rest ->
                let new_config = run_command (stk, env, trace, List.tl commands) cmd in
                run_program new_config 
  )


let eval_prog config = 
  match run_program config with 
  | (_, _, trace, _) -> Some trace 
  

let interp = fun x -> 
  let stack = [] in
  let trace = [] in
  let env = [] in
  match (parse parse_prog x) with 
    | Some command_list -> eval_prog (stack, trace, env, command_list)
    | None -> None



(* END OF PROJECT CODE *)



(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)


let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t



let _ = main ()


