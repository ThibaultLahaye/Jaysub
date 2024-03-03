module Expression = Expression

(* Constant folding and peephole optimizations *)

(** [fold_expression expr] performs constant folding on the given expression. This function looks for a set of patterns
in the expressions an then applies a transofrmation or evaluated the expression. Only constant expressions are evaluated
recursivley. Meaning that 4*3*5*6 is simplified to 360, but algebraic expressions like x+x+x are not simplified to 3*x 
behond the simple algebraic expressions listed below. **)
let rec fold_expression (expr : Ast.expression) : Ast.expression =
  match expr with
  | Ast.Ident id -> Ast.Ident id
  | Ast.Constant c -> Ast.Constant c
  | Ast.Binop (e1, op, e2) ->
    let e1 = fold_expression e1 in
    let e2 = fold_expression e2 in
    match (e1, op, e2) with
    (* Plus Optimizations *)
    | (Ast.Ident id1, Ast.Plus, Ast.Constant 0) -> Ast.Ident id1                (* x + 0 = x *)
    | (Ast.Constant 0, Ast.Plus, Ast.Ident id1) -> Ast.Ident id1                (* 0 + x = x *)
    (* | (Ast.Ident id1, Ast.Plus, Ast.Ident id2) when id1 = id2 -> 
      Ast.Binop (Ast.Ident id1, Ast.Mult, Ast.Constant 2) *)                    (* x + x = 2x *) 
    (* Min Optimizations *) 
    | (Ast.Ident id1, Ast.Min, Ast.Constant 0) -> Ast.Ident id1                 (* x - 0 = x *)
    | (Ast.Constant 0, Ast.Min, Ast.Ident id1) -> Ast.Ident id1                 (* 0 - x = x *)
    | (Ast.Ident id1, Ast.Min, Ast.Ident id2) when id1 = id2 -> Ast.Constant 0  (* x - x = 0 *)
    (* Mult Optimizations *)
    | (Ast.Ident _, Ast.Mult, Ast.Constant 0) -> Ast.Constant 0                (* x * 0 = 0 *)
    | (Ast.Constant 0, Ast.Mult, Ast.Ident _) -> Ast.Constant 0                (* 0 * x = 0 *)
    | (Ast.Ident id1, Ast.Mult, Ast.Constant 1) -> Ast.Ident id1                 (* x * 1 = x *)
    | (Ast.Constant 1, Ast.Mult, Ast.Ident id1) -> Ast.Ident id1                 (* 1 * x = x *)
    (* Div Optimizations *)
    | (Ast.Ident id1, Ast.Div, Ast.Ident id2) when id1 = id2 -> Ast.Constant 1                 (* x / x = 1 *)
    | (Ast.Ident id1, Ast.Div, Ast.Constant 1) -> Ast.Ident id1                 (* x / 1 = x *)
    | (Ast.Constant 0, Ast.Div, Ast.Ident _) -> Ast.Constant 0                (* 0 / x = 0 *)
    | (Ast.Ident _, Ast.Div, Ast.Constant 0) -> failwith "Division by zero"   (* x / 0 = error *)
    (* Eq Optimizations*)
    | (Ast.Ident id1, Ast.Eq, Ast.Ident id2) when id1 = id2 -> Ast.Constant 1                  (* x = x => 1 *)
    (* Neq Optimizations*)
    | (Ast.Ident id1, Ast.NEq, Ast.Ident id2) when id1 = id2 -> Ast.Constant 0                 (* x != x => 0 *)
    (* Const Optimizations *)
    | (Ast.Constant c1, binop, Ast.Constant c2) -> 
      Ast.Constant (Expression.eval_binop c1 binop c2)
    (* Nothing to Optimize *)
    | _ -> Ast.Binop (e1, op, e2)

(** [fold_statement stmt] performs constant folding on the given statement. By applying constant folding
    on their substatements. *)
let rec fold_statement (stmt : Ast.statement) : Ast.statement =
  match stmt with
  | Ast.IfStmt (c1, t, f, c2) ->
    Ast.IfStmt (fold_expression c1, List.map fold_statement t, List.map fold_statement f, fold_expression c2)
  | Ast.CallStmt id -> Ast.CallStmt id
  | Ast.UncallStmt id -> Ast.UncallStmt id
  | Ast.ModStmt (id, bop, e) -> Ast.ModStmt (id, bop, fold_expression e)
  | Ast.SwapStmt (id1, id2) -> Ast.SwapStmt (id1, id2)
  | Ast.SkipStmt -> Ast.SkipStmt
  | Ast.LoopStmt (c1, lb, c2) -> Ast.LoopStmt (fold_expression c1, List.map fold_statement lb, fold_expression c2)

(** [fold_proc proc] performs constant folding on the given procedure. *)
let fold_proc (proc: Ast.procedure): Ast.procedure =
  let (id, stmts) = proc in 
  (id, List.map fold_statement stmts)

(** [fold_procs procs] performs consstant folding on the list of procedures*)
let fold_procs (procs: Ast.procedure list): Ast.procedure list = 
  (List.map fold_proc procs)

(** [program prog] performs constant folding and peephole optimizations on the entire program. 
    fold_procs and fold_proc was only introduced to the file structure more coherent with the other files. *)
let program (prog: Ast.program): Ast.program = 
  let (variables, procs) = prog in 
  (variables, fold_procs procs)
