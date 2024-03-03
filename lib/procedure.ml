(* Helper functions *)

(** Find Procedure by Identifier

   [find id procs] searches for a procedure with the specified identifier [id] in a list of procedures [procs].
   If the procedure is found, it is returned. Otherwise, a failure is raised. The procedures [procs] represent
   the procedures in the program.

*)
let find (id : Ast.identifier) (procs : Ast.procedure list) : Ast.procedure =
    try
        List.find (fun (proc_id, _) -> proc_id = id) procs  
    with
    | Not_found -> failwith ("Procedure not found: " ^ id)

(** Find Last Procedure

   [last procs] finds the last procedure in a list of procedures [procs].
   The last procedure is then used as the entrypoint of the program. 

*)
let rec last (procs: Ast.procedure list): Ast.procedure = 
    match procs with
    | [] -> failwith ("The program does not contain any procedures")
    | [x] -> x
    | _ :: tail -> last tail

(** Forward Evaluation of Procedures (feval) *)

(** Recursive Evaluation of Statements

   [feval_statements store stmts procs] recursively evaluates a list of statements [stmts] within the context 
   of a given given porgram represented by the store and list of procedures.

*)
let rec feval (store : Store.store) ((_, stmts) : Ast.procedure) (procs: Ast.procedure list) : Store.store =
    feval_statements store stmts procs
and feval_statements (store : Store.store) (stmts : Ast.statement list) (procs: Ast.procedure list) : Store.store =
    List.fold_left (feval_statement procs) store stmts
and feval_statement (procs: Ast.procedure list) (store : Store.store) (stmt : Ast.statement) : Store.store =
    match stmt with
    | Ast.IfStmt (c1, true_branch, false_branch, c2 ) ->
        let updated_store = 
          if Expression.eval store c1 <> 0 then
            feval_statements store true_branch procs
          else
            feval_statements store false_branch procs
        in if (Expression.eval updated_store c2 = 0) = (Expression.eval store c1 = 0) then 
          updated_store
        else 
          failwith "Undefined If Behavior"
    | Ast.CallStmt id ->
        feval store (find id procs) procs
    | Ast.UncallStmt id ->
        feval store (Invert.invert_procedure (find id procs)) (List.map Invert.invert_procedure procs)
    | Ast.ModStmt (id, binop, expr) ->
        let current_value = Store.read id store in
        let new_value = Expression.eval store expr in
        let updated_value =
            match binop with
            | Ast.Plus -> current_value + new_value
            | Ast.Min -> current_value - new_value
            | Ast.Mult -> current_value * new_value
            | Ast.Div -> current_value / new_value
            | _ -> failwith "Unsupported binary operation for modification"
        in
            Store.write id updated_value store
    | Ast.SwapStmt (id1, id2) ->
        let value1 = Store.read id1 store in
        let value2 = Store.read id2 store in
        let updated_store =
            Store.write id1 value2 (Store.write id2 value1 store)
        in
            updated_store
    | Ast.SkipStmt -> store
    | Ast.LoopStmt (pre_cond, loop_body, post_cond) ->
      if Expression.eval store pre_cond <> 0 then
        let rec evaluate_loop store =
          let updated_store = feval_statements store loop_body procs in
          let post_condition_value = Expression.eval updated_store post_cond in
          if post_condition_value = 0 then
            if Expression.eval updated_store pre_cond = 0 then 
              evaluate_loop updated_store
            else 
              failwith "Undefined Loop Behavior"
          else
              updated_store
        in
        evaluate_loop store
      else
        failwith "Precondition must be true"


(** Backwards Evaluation of Procedures (beval) *)

(** Recursive Backward Evaluation of Statements

   [beval_statements store stmts procs] recursively evaluates a list of statements [stmts] backward within the context 
   of a given program represented by the store and list of procedures.

*)
let rec beval (store : Store.store) ((_, stmts) : Ast.procedure) (procs: Ast.procedure list) : Store.store =
  beval_statements store stmts procs
and beval_statements (store : Store.store) (stmts : Ast.statement list) (procs: Ast.procedure list) : Store.store =
  List.fold_right (beval_statement procs) stmts store
and beval_statement (procs: Ast.procedure list) (stmt : Ast.statement) (store : Store.store) : Store.store =
  match stmt with
  | Ast.IfStmt (c1, true_branch, false_branch, c2 ) ->
      let updated_store = 
        if Expression.eval store c2 <> 0 then
          beval_statements store true_branch procs
        else
          beval_statements store false_branch procs
        in if (Expression.eval updated_store c2 = 0) = (Expression.eval store c1 = 0) then 
          updated_store
        else 
          failwith "Undefined If Behavior"
  | Ast.CallStmt id ->
      beval store (find id procs) procs
  | Ast.UncallStmt id ->
      Store.to_string store |> Printf.printf "%s\n";
      beval store (Invert.invert_procedure (find id procs)) (List.map Invert.invert_procedure procs)
  | Ast.ModStmt (id, binop, expr) ->
      let current_value = Store.read id store in
      let new_value = Expression.eval store expr in
      let updated_value =
          match binop with
          | Ast.Plus -> current_value - new_value
          | Ast.Min -> current_value + new_value
          | Ast.Mult -> current_value / new_value
          | Ast.Div -> current_value * new_value
          | _ -> failwith "Unsupported binary operation for modification"
      in
          Store.write id updated_value store
  | Ast.SwapStmt (id1, id2) ->
      let value1 = Store.read id1 store in
      let value2 = Store.read id2 store in
      let updated_store =
          Store.write id1 value2 (Store.write id2 value1 store)
      in
          updated_store
  | Ast.SkipStmt -> store
  | Ast.LoopStmt (pre_cond, loop_body, post_cond) ->
    if Expression.eval store post_cond <> 0 then
      let rec evaluate_loop store =
        let updated_store = beval_statements store loop_body procs in
        let pre_condition_value = Expression.eval updated_store pre_cond in
        if pre_condition_value = 0 then
          if Expression.eval updated_store post_cond = 0 then 
            evaluate_loop updated_store
          else 
            failwith "Undefined Loop Behavior"
        else
            updated_store
      in
      evaluate_loop store
    else
      failwith "Precondition must be true"
