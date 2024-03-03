(** Evaluation of Binary Operations

   [eval_binop v1 op v2] evaluates a binary operation [op] with operands [v1] and [v2].
   It supports arithmetic operations (Plus, Min, Mult, Div) and comparison operations (Eq, NEq).

*)
let eval_binop (v1: int) (op: Ast.binop) (v2: int): int = 
  match op with
  | Ast.Plus -> v1 + v2
  | Ast.Min -> v1 - v2
  | Ast.Mult -> v1 * v2
  | Ast.Div -> v1 / v2
  | Ast.Eq -> if v1 = v2 then 1 else 0
  | Ast.NEq -> if v1 <> v2 then 1 else 0

(** Recursive Evaluation of Expressions

   [eval store expr] recursively evaluates an expression [expr] within the context of a given store.

*)
let rec eval (store : Store.store) (expr : Ast.expression) : int =
    match expr with
    | Ast.Ident id -> Store.read id store
    | Ast.Constant i -> i
    | Ast.Binop (e1, op, e2) ->
        let v1 = eval store e1 in
        let v2 = eval store e2 in
        eval_binop v1 op v2