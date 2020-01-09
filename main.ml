open AlgosTypes
open Printf

let ($) f x = f x
let (<.>) f g x = f (g x)

let rec mapExpr v e =
  match e with
    One -> Var v
  | Zero -> Zero
  | Var s when s = v -> Var s
  | Var s -> Mult (Var v, Var s)
  | Inv (Var s) when s = v -> Inv (Var s)
  | Inv (e1) -> Inv (mapExpr v e1)
  | Plus (e1,e2) -> Plus ((mapExpr v e1), (mapExpr v e2))
  | Mult (e1,Var s) as e -> 
    if s = v then 
      e
    else
      Mult (e1, Mult (Var s, Var v))
  | Mult (Var s, e1) as e-> 
    if s = v then 
      e
    else
      Mult (e1, Mult (Var s, Var v))
  | Mult (e1,e2) -> Mult ((mapExpr v e1), (mapExpr v e2))

let rec isProductForm e =
  match e with 
    Var v1 -> true
  | Zero -> true
  | One -> true
  | Inv (Var v1) -> true
  | Inv (Zero) -> true
  | Inv (One) -> true
  | Mult (e1, e2) when isProductForm e1 && isProductForm e2 -> true
  | _ -> false

let rec isSumProductForm e =
  match e with 
    Plus (e1, e2) when isProductForm e1 && isProductForm e2 -> true
  | _ -> false

let rec normalize e =
  match e with
    Mult (Plus (e1,e2), Plus (e3,e4)) -> 
      let e1' = normalize (Mult (e1,e3)) in
      let e2' = normalize (Mult (e1,e4)) in
      let e3' = normalize (Mult (e2,e3)) in
      let e4' = normalize (Mult (e2,e4)) in
      Plus (Plus (e1',e2'), Plus (e3', e4'))
  | Mult (Plus (e1,e2), Mult (e3,e4)) -> 
    let e1' = normalize (Mult (e1, Mult (e3,e4))) in
    let e2' = normalize (Mult (e2, Mult (e3,e4))) in
    Plus (e1',e2')
  | Mult (Var v1, (Mult (Var v2, Var v3) as e1)) as e' ->
    if v1 = v2 || v1 = v3 then
      e1
    else
      e'
  | Mult (Var v1, (Mult (e1, e2))) ->
    let e1' = normalize (Mult (e1, e2)) in
    normalize (Mult (Var v1, e1'))
  | Mult (Var v1, Var v2) as e1 -> 
    if v1 = v2 then Var v1 else e1
  | Mult (Var v1, (Inv (Var v2))) -> 
    if v1 = v2 then (Inv (Var v1)) else Inv (Mult (Var v1, Var v2))
  | Mult ((Inv (Var v2)), Var v1) -> 
    if v1 = v2 then (Inv (Var v1)) else Inv (Mult (Var v1, Var v2))
  | Mult (Var v1, One) -> Var v1
  | Mult (One, Var v1) -> Var v1
  | Mult (Var v1, Zero) -> Zero
  | Mult (Zero, Var v1) -> Zero
  | Plus (e1,e2) -> 
    let e1' = normalize e1 in 
    let e2' = normalize e2 in 
    Plus (e1', e2')
  | e1 -> e1

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec postfix_string_to_expr stack line =
  match (line, stack) with
  | ('+' :: x, a :: b :: y) ->
    postfix_string_to_expr (Plus (a, b) :: y) x
  | ('*' :: x, a :: b :: y) ->
    postfix_string_to_expr (Mult (a, b) :: y) x
  | ('-' :: x, a :: y) ->
    postfix_string_to_expr (Inv a :: y) x
  | ('0' :: x, y) ->
    postfix_string_to_expr (Zero :: y) x
  | ('1' :: x, y) ->
    postfix_string_to_expr (One :: y) x
  | (v :: x, y) ->
    let name = sprintf "%c" v in
    postfix_string_to_expr (Var name :: y) x
  | ([], [f]) ->
    f
  | ([], _) ->
    failwith "Invalid expression"

let rec parse () =
  print_string "<rpnben> ";
  flush stdout;
  try
    let line = input_line stdin in
    let list = explode line in
    let e = postfix_string_to_expr [] list in
    printf "%s\n" (expr2string e);
    printf "%s\n" (expr2string (normalize e));
    flush stdout;
    parse ();
  with
    End_of_file -> 
      begin 
	print_string "Leaving rpnben.\n";
	flush stdout
      end

let _ = 
  begin
    print_string $ "Welcome to rpnben:\n";
    parse ();
    exit 0;
  end
