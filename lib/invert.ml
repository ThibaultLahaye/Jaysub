(** Inversion of Binary Operations

   [invert_binop binop] inverts a binary operation [binop]. 
   The inversion is based on swapping the operands for arithmetic operations 
   and negating for comparison operations.

*)
let invert_binop (binop : Ast.binop) : Ast.binop =
  match binop with
  | Ast.Plus -> Ast.Min
  | Ast.Min -> Ast.Plus
  | Ast.Mult -> Ast.Div
  | Ast.Div -> Ast.Mult
  | Ast.Eq -> Ast.NEq
  | Ast.NEq -> Ast.Eq

(** Inversion of Statements

   [invert_statement stmt] inverts a statement [stmt]. 
   The inversion involves swapping conditions in if statements, 
   changing the direction of loops, and inverting binary operations.

*)
let rec invert_statement (stmt : Ast.statement) : Ast.statement =
  match stmt with
  | Ast.IfStmt (cond, true_branch, false_branch, post_cond) ->
      Ast.IfStmt (post_cond, invert_statements true_branch, invert_statements false_branch, cond)
  | Ast.CallStmt id -> Ast.CallStmt id
  | Ast.UncallStmt id -> Ast.UncallStmt id
  | Ast.ModStmt (id, binop, expr) -> Ast.ModStmt (id, invert_binop binop, expr)
  | Ast.SwapStmt (id1, id2) -> Ast.SwapStmt (id2, id1)
  | Ast.SkipStmt -> Ast.SkipStmt
  | Ast.LoopStmt (pre_cond, loop_body, post_cond) ->
      Ast.LoopStmt (post_cond, invert_statements (loop_body), pre_cond)

(** Inversion of Lists of Statements

   [invert_statements stmts] inverts a list of statements [stmts].

*)
and invert_statements (stmts : Ast.statement list) : Ast.statement list =
  List.map invert_statement (List.rev stmts)

(** Inversion of Procedures

   [invert_procedure proc] inverts a procedure [proc] by inverting its list of statements.
*)
let invert_procedure (proc : Ast.procedure) : Ast.procedure =
  let (id, stmts) = proc in
  let inverted_stmts = List.map invert_statement (List.rev stmts) in
  (id, inverted_stmts)