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
 | Plus of expr * expr
 | Mult of expr * expr

let rec vars1 expr varset =
  match expr with
    Var s -> VarSet.add s varset
  | Inv e1 -> vars1 e1 varset
  | Plus (e1,e2) -> VarSet.union (vars1 e1 varset) (vars1 e2 varset)
  | Mult (e1,e2) -> VarSet.union (vars1 e1 varset) (vars1 e2 varset)
  | _ -> VarSet.empty

let vars expr = vars1 expr VarSet.empty

let varset2string varset = VarSet.fold (fun element seed -> seed ^ " " ^ element) varset ""

let rec expr2string expr =
  match expr with
    One -> "1"
  | Zero -> "0"
  | Var v -> v
  | Inv expr1 -> "(" ^ "−" ^ (expr2string expr1) ^ ")"
  | Plus (expr1,expr2) -> "(" ^ (expr2string expr1) ^ " + " ^ (expr2string expr2) ^ ")"
  | Mult (expr1,expr2) -> "(" ^ (expr2string expr1) ^ " × " ^ (expr2string expr2) ^ ")"

let print_expr expr = printf "%s\n" (expr2string expr)
