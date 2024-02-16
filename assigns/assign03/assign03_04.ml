(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { num_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let make_matrix num_rows num_cols rows = 
  let my_matrix = {
    num_rows = num_rows;
    num_cols = num_cols;
    rows = rows;
  } in 
  my_matrix

let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  if (List.length rs) = 0 then
    Error ZeroRows

  else
    let head = List.hd rs in
    let row_len = List.length head in
    let rec check_even_rows rs_rec row_len row_count = 
    match rs_rec with 
    | [] -> (
        if (row_len = 0) then 
          Error ZeroCols
        else 
          Ok (make_matrix row_count row_len rs)
    )
    | x :: xs -> (
        if (List.length x) = row_len then 
          check_even_rows xs row_len (row_count+1)
        else 
          Error UnevenRows
    )
    in
    
        check_even_rows rs row_len 0



let transpose (m : 'a matrix) : 'a matrix =
  let row_len = m.num_rows in 
  let col_len = m.num_cols in
  (* Adjusted to directly handle single row or single column matrices *)
  if col_len = 1 then
    let combined = List.flatten m.rows in
    make_matrix col_len row_len [combined]
  else
  let rec transp_builder curr_row start_matrix end_matrix = 
    if (List.length curr_row) = row_len then
      transp_builder [] start_matrix (curr_row :: end_matrix)
    else 
      let initial_row = List.hd start_matrix in 
      let tail_matrix = List.tl start_matrix in
      match initial_row with
      | [] -> List.rev end_matrix
      | x :: xs -> transp_builder (List.rev(x :: curr_row)) (List.rev (xs :: tail_matrix)) end_matrix

  in 
  let transp_list = (transp_builder [] m.rows []) in 
  make_matrix col_len row_len transp_list



let dot_product row col =
  List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 row col

let rec get_col n matrix =
  match matrix with
  | [] -> []
  | row :: rest -> (List.nth row n) :: get_col n rest

let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  if m.num_cols <> n.num_rows then
    Error MulMismatch
  else
    let result_rows =
      List.map (fun m_row ->
        List.init n.num_cols (fun i ->
          dot_product m_row (get_col i n.rows)
        )
      ) m.rows
    in
    Ok {
      num_rows = m.num_rows;
      num_cols = n.num_cols;
      rows = result_rows;
    }

