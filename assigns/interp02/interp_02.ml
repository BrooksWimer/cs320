(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_lower_case c = 'a' <= c && c <= 'z'

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
let ( let* ) = bind

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

(* REQUIRED TYPES *)

type ident = string

type const
  = Num of int
  | Bool of bool

type command
  = Push of const | Trace
  | Add | Mul | Div
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of program | Call | Return
  | Debug of string

and program = command list

and bindings = (ident * value) list

and value
  = Const of const
  | Clos of
      { def_id : int
      ; captured : bindings
      ; prog : program
      }

type record =
  { id : int
  ; local : bindings
  ; called_def_id : int
  ; return_prog : program
  }

type stack = value list
type trace = string list
type env
  = Global of bindings
  | Local of record * env

(* get the id of the topmost record *)
let local_id = function
  | Global _ -> 0
  | Local (r, _) -> r.id

(* convert a value to a string *)
let to_string v =
  match v with
  | Const (Bool true) -> "True"
  | Const (Bool false) -> "False"
  | Const (Num n) -> string_of_int n
  | Clos _ -> "<Closure>"

(* PARSING *)

let parse_ident =
  map2
    (fun c cs -> implode (c :: cs))
    (satisfy is_lower_case)
    (many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')))

let parse_int =
  let mk_int sign cs =
    let abs = int_of_string (implode cs) in
    if Option.is_none sign
    then abs
    else -abs
  in
  map2
    mk_int
    (optional (char '-'))
    (many1 (satisfy is_digit))

let parse_bool =
  (str "True" >| true) <|> (str "False" >| false)

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode
(*
let parse_clos =
  char '[' >>
  parse_int >>= fun def_id ->
  char ',' >>
  parse_bindings >>= fun captured ->
  char ',' >>
  parse_prog_rec () >>= fun prog ->
  char ']' >>
  pure (Clos { def_id; captured; prog })

let consume_header = 
    map (fun c -> Fun c)
    (keyword "(" >> 
    many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')) 
    >> keyword "):" >> keyword ";")

    let* _ = keyword "(" in
    let* header = parse_prog_rec () in
    let* _ = keyword "):" in
    let* body = parse_prog_rec () in
    let* _ = char ';' in
    pure (Fun body)



let parse_val = 
  (parse_const >|= fun n -> Const n) <|> 
  (parse_clos >|= fun b -> Clos b)
*)

let parse_const =
  (parse_int >|= fun n -> Num n) <|> 
  (parse_bool >|= fun b -> Bool b)

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let rec parse_com () =
  let parse_fun = 
    let* _ = keyword ":" in
    let* header = parse_prog_rec () in
    let* _ = keyword ";" in
    pure (Fun header)
  in
  let parse_if =
    let* _ = keyword "?" in
    let* ifc = parse_prog_rec () in
    let* _ = keyword ";" in
    let* elsec = parse_prog_rec () in
    let* _ = char ';' in
    pure (If (ifc, elsec))
  in
  let parse_while =
    let* _ = keyword "While" in
    let* check = parse_prog_rec () in
    let* _ = keyword ";" in
    let* body = parse_prog_rec () in
    let* _ = char ';' in
    pure (While (check, body))
  in
  let parse_push =
    parse_const >|= fun c -> Push c
  in 
  let parse_bind =
  (keyword "|>" >> parse_ident) >|= fun id -> Bind id
  in

  choice
    (* TODO: Add more alternatives *)
    [ parse_fun
    ; parse_while
    ; parse_if
    ; parse_bind 
    ; parse_push
    ; parse_ident >|= (fun s -> Fetch s)
    ; parse_debug >|= (fun s -> Debug s)
    ; keyword "+" >> pure Add
    ; keyword "*" >> pure Mul
    ; keyword "/" >> pure Div
    ; keyword "<" >> pure Lt
    ; keyword "=" >> pure Eq
    ; keyword "&&" >> pure And
    ; keyword "||" >> pure Or
    ; keyword "~" >> pure Not
    ; keyword "." >> pure Trace
    ; keyword "#" >> pure Call
    ; keyword "Return" >> pure Return
  
    ]

and parse_prog_rec () =
  many (rec_parser parse_com << ws)

let parse_prog = parse (ws >> parse_prog_rec ())

let test = parse_prog ": 1 |> x
; |> g
"



(* FETCHING AND UPDATING *)

let rec find_env_by_id id_to_find env =
  match env with
  | Global _ -> env  (* If we reach Global, stop searching *)
  | Local (record, enclosing_env) ->
      if record.id = id_to_find then env
      else find_env_by_id id_to_find enclosing_env
  

(* fetch the value of `x` in the environment `e` *)
let rec fetch_env e x =  
  match e with
  | Global(bindings) -> 
      List.assoc_opt x bindings  
  | Local(record, enclosing_env) ->
      match List.assoc_opt x record.local with
      | Some value -> Some value  (* Found in local bindings *)
      | None -> fetch_env (find_env_by_id record.called_def_id enclosing_env) x


let rec update_or_add bindings var_name new_value =
  match bindings with 
  | [] -> [(var_name, new_value)]
  | (id, var) :: xs -> ( 
    if id = var_name then (id, new_value) :: xs 
    else (id, var) :: (update_or_add xs var_name new_value)
  )

let update_env env var_name new_value =
  match env with
  | Global(bindings) ->
      Global(update_or_add bindings var_name new_value)
  | Local(record, outer_env) ->
      let updated_bindings = update_or_add record.local var_name new_value in
      Local({record with local = updated_bindings}, outer_env)

(* EVALUTION *)

(* make the panic configuration given a configuration *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

let rec eval_step (c : stack * env * trace * program) =
  match c with
  (* Push *)
  | s, e, t, Push c :: p -> Const c :: s, e, t, p
  (* Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow (. on empty)"
  (* Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Add :: _ -> panic c "stack underflow (+ on empty)"
  (* Mul *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Mul :: _ -> panic c "type error (* on non-integers)"
  | _ :: [], _, _, Mul :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, Mul :: _ -> panic c "stack underflow (* on empty)"
  (* Div *)
  | Const (Num m) :: Const (Num 0) :: s, e, t, Div :: p -> panic c "type error (/ by zero)"
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p -> Const (Num (m / n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Div :: _ -> panic c "type error (/ on non-integers)"
  | _ :: [], _, _, Div :: _ -> panic c "stack underflow (/ on single)"
  | [], _, _, Div :: _ -> panic c "stack underflow (/ on empty)"
  (* Lt *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Lt :: p -> Const (Bool (if m < n then true else false)) :: s, e, t, p
  | _ :: _ :: _, _, _, Lt :: _ -> panic c "type error (< on non-integers)"
  | _ :: [], _, _, Lt :: _ -> panic c "stack underflow (< on single)"
  | [], _, _, Lt :: _ -> panic c "stack underflow (< on empty)"
  (* Eq *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Eq :: p -> Const (Bool (if m = n then true else false)) :: s, e, t, p
  | _ :: _ :: _, _, _, Eq :: _ -> panic c "type error (= on non-integers)"
  | _ :: [], _, _, Eq :: _ -> panic c "stack underflow (= on single)"
  | [], _, _, Eq :: _ -> panic c "stack underflow (= on empty)"
  (* And *)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, And :: p -> Const (Bool (m && n)) :: s, e, t, p
  | _ :: _ :: _, _, _, And :: _ -> panic c "type error (&& on non-booleans)"
  | _ :: [], _, _, And :: _ -> panic c "stack underflow (&& on single)"
  | [], _, _, And :: _ -> panic c "stack underflow (&& on empty)"
  (* Or *)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, Or :: p -> Const (Bool (m || n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Or :: _ -> panic c "type error (|| on non-booleans)"
  | _ :: [], _, _, Or :: _ -> panic c "stack underflow (|| on single)"
  | [], _, _, Or :: _ -> panic c "stack underflow (|| on empty)"
  (* Not *)
  | Const (Bool m) :: s, e, t, Not :: p -> Const (Bool (not m)) :: s, e, t, p
  | _ :: _, _, _, Not :: _ -> panic c "type error (not on non-boolean)"
  | [], _, _, Not :: _ -> panic c "stack underflow (not on empty)"
  (* If-Else *)
  | Const (Bool true) :: s, e, t, If (q1, q2) :: p -> s, e, t, q1 @ p
  | Const (Bool false) :: s, e, t, If (q1, q2) :: p -> s, e, t, q2 @ p
  | _ :: _ :: _, _, _, If (q1, q2) :: _ -> panic c "type error (If-Else on non-boolean)"
  | [], _, _, If (q1, q2) :: _ -> panic c "stack underflow (If-Else on empty)"
  (* While *)
  | s, e, t, While (q1, q2) :: p -> s, e, t, q1 @ [If (q2 @ [While (q1, q2)], [])] @ p
  (* Fetch *)
  | s, e, t, Fetch x :: p -> (
        match fetch_env e x with 
        | Some x -> x :: s, e, t, p
        | None -> panic c "fetch error, val is not bound in enviornment"
  )
  (* Bind *)
  | Const (Bool v) :: s, e, t, Bind (var_name) :: p -> s, (update_env e var_name (Const (Bool v))), t, p
  | Const (Num v) :: s, e, t, Bind (var_name) :: p -> s, (update_env e var_name (Const (Num v))), t, p
  | Clos v :: s, e, t, Bind var_name :: p -> s, (update_env e var_name (Clos v)), t, p
  | [], _, _, Bind var_name :: _ -> panic c "stack underflow (bind on empty)"
  (* Call *)
  | Clos {def_id; captured; prog} :: s, e, t, Call :: p -> (
                          let new_record = { 
                            id = def_id+1
                          ; local = captured
                          ; called_def_id = def_id
                          ; return_prog = p
                           } in
                          s, Local(new_record, e), t, prog
  )
  | _, _, _, Call :: p -> panic c "type error (call on non-closure)"
  (* Function *) 
  | s, e, t, Fun prog :: p -> 
        Clos {def_id = local_id e; captured = []; prog = prog} :: s, e, t, p 
  (* Return *)
  | Clos {def_id; captured; prog} :: [], Local (record, remaining_env), t, Return :: p ->(
      if (record.id) = def_id then 
      let new_closure = Clos {def_id = record.called_def_id; captured = record.local; prog=prog} in 
      [new_closure], remaining_env, t, record.return_prog
      else 
      [Clos {def_id; captured; prog}], remaining_env, t, record.return_prog
  )
  | x :: [], Local (record, remaining_env), t, Return :: p -> [x], remaining_env, t, record.return_prog
  | [], Local (record, remaining_env), t, Return :: p -> [], remaining_env, t, record.return_prog
  | [], Local (record, remaining_env), t, [] -> [], remaining_env, t, record.return_prog
  | x :: y :: s, Local (record, remaining_env), t, Return :: p -> panic c "return error, too many values in stack"
  | x :: s, Local (record, remaining_env), t, [] -> panic c "return error, value in stack without return call" 
  | s, Global _, t, Return :: p -> panic c "return error, can't return in global enviornment" 


 

let rec eval c =
  match c with
  | (s, Global e, t, []) -> t
  | _ -> eval (eval_step c)



let rec eval_prog p = eval ([], Global [], [], p)
let interp s = Option.map eval_prog (parse_prog s)

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

(*
let test_string = "
(f):
  0 |> x
  (g):
    (h):
      x .
    ;
    Return
  ; #
  Return
; |> f


f # |> q

(a):
  1 |> x
  q #
; #
"

let test_string2 = " (fib): |> n
  (if)
    n 0 =
    n 1 =
    || ?
  (then)
    n Return ;
  (else)
    -1 n + fib # |> x
    -2 n + fib # |> y
    x y + Return ;
; |> fib

8 fib # .

(print_fib): |> n
  0 |> count
  While count n = ~ ;
    count fib # .
    count 1 + |> count
  ;
; |> print_fib

8 print_fib# "

let test_string3 = "(sqrt): |> n
  (if) 0 n < ? Return ; ;
  (if) 0 n = ? 0 Return ; ;
  0 |> i
  While n 1 + i < ;
    (if) i i * n < ?
      i -1 + Return ;
    (else)
      i 1 + |> i
    ;
  ;
; |> sqrt

(is_prime): |> n
  (if) 2 n < ? False Return ; ;
  2 |> i
  n sqrt # |> s
  While s 1 + i < ;
    (if) i n / i * n = ?
      False Return ;
    (else)
      i 1 + |> i
    ;
  ;
  True Return
; |> is_prime

11 is_prime # . "


let test3 = interp test_string



(*
let rec shadowing curr_bindings old_bindings = 
  match curr_bindings with 
  | [] -> old_bindings 
  | (var_name, val) :: remaining_bindings -> shadowing remaining_bindings (update_or_add old_bindings var_name val)
     
     | Clos [id; bindings; closure_prog] :: [], Local (record, remaining_env) :: e, t, Return :: p ->
      if id = record.id then 
      
     *) 


let c = match (parse_prog test_string3) with 
  | Some x -> ([], Global [], [], x)
  | None -> ([], Global [], [], [])

(
      match eval_step (s, e, t, q1) with 
      | Const (Bool true) :: s, _, _, _ -> s, e, t, q2 @ While (q1, q2) :: p 
      | Const (Bool false) :: s, _, _, _ -> s, e, t, p 
      | _ :: _, _, _, _ -> panic c "type error (While on non-boolean)"
      | [], _, _, _ -> panic c "stack underflow (while on empty)"
  )


let testing_prog = ([],
   Global
    [("f",
      Clos
       {def_id = 0; captured = [];
        prog =
         [Push (Num 0); Bind "x"; Fun [Fun [Fetch "x"; Trace]; Return]; Call;
          Return]})],
   [], [Fetch "f"; Call])

let test2 = eval_step testing_prog




let test2 = match (parse_prog test_string) with 
  | Some x -> eval_step ([], Global [], [], x)
  | None -> ([], Global [], [], [])








let test3 = eval_step ([Const (Bool false)], Global [], [],
   [If ([Push (Num 4); Trace], [Push (Num 5); Trace])])

let rec eval_step2 (c : stack * env * trace * program) =
  match c with
  | Const (Bool v) :: s, e, t, Bind (var_name) :: p -> s, (update_env e var_name (Const (Bool v))), t, p
  | Const (Num v) :: s, e, t, Bind (var_name) :: p -> s, (update_env e var_name (Const (Num v))), t, p
  | Clos v :: s, e, t, Bind var_name :: p -> s, (update_env e var_name (Clos v)), t, p
  | [], _, _, Bind var_name :: _ -> panic c "stack underflow (bind on empty)"

let test2 = match (parse_prog test_string2) with 
  | Some x -> eval_step ([], Global [], [], x)
  | None -> ([], Global [], [], [])

let test3 = eval_step2 ([Const (Num 2)], Global [], [], [Bind "x"])



let test2 = interp test_string2
*)
(* MAIN *)



(* END OF FILE *)
