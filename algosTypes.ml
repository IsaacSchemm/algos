open Printf

let ($) f x = f x
let (<.>) f g x = f (g x)

module VarSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = string
  end )

 type expr = 
   One
 | Zero
 | Var of string
 | Inv of expr
 | Not of expr
 | Xor of expr * expr
 | Plus of expr * expr
 | Mult of expr * expr

let rec expr2string expr =
  match expr with
    One -> "1"
  | Zero -> "0"
  | Var v -> v
  | Inv expr1 -> "(" ^ "−" ^ (expr2string expr1) ^ ")"
  | Not expr1 -> "" ^ "~" ^ (expr2string expr1) ^ ""
  | Xor (expr1,expr2) -> "(" ^ (expr2string expr1) ^ " ^ " ^ (expr2string expr2) ^ ")"
  | Plus (expr1,expr2) -> "(" ^ (expr2string expr1) ^ " + " ^ (expr2string expr2) ^ ")"
  | Mult (expr1,expr2) -> "(" ^ (expr2string expr1) ^ " × " ^ (expr2string expr2) ^ ")"

let print_expr expr = printf "%s\n" (expr2string expr)
