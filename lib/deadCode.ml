module Procedure = Procedure

(** [replace procs new_proc] replaces the existing procedure in [procs] with the given [new_proc]. 
    It searches for a procedure in [procs] with the same identifier as [new_proc] and 
    replaces it with [new_proc] in the list.
*)
let replace (procs: Ast.procedure list) (new_proc: Ast.procedure): Ast.procedure list =
  let (id, _) = new_proc in
  let rec replace_proc (procs: Ast.procedure list): Ast.procedure list =
    match procs with
    | [] -> failwith "replace: Procedure not found"
    | (existing_id, _) :: tl when existing_id = id -> new_proc :: tl 
    | hd :: tl -> hd :: replace_proc tl
  in replace_proc procs

(** [rebuild_proc proc other_procs acc] recursively rebuilds the given procedure [proc] and updates it based on the call graph. *)
let rec rebuild_proc (proc: Ast.procedure) (other_procs: Ast.procedure list) (acc: Ast.procedure list): Ast.procedure list =
  let (id, stmts) = proc in
  let (rebuilt_stmts, new_acc) = rebuild_statements stmts other_procs acc in
  let new_new_acc = replace new_acc (id, rebuilt_stmts) in
  new_new_acc

(** [rebuild_statements stmts other_procs acc] rebuilds the given list of statementss [stmts] and updates the list of procedures based on the call graph. *)
and rebuild_statements (stmts: Ast.statement list) (other_procs: Ast.procedure list) (acc: Ast.procedure list) : Ast.statement list * Ast.procedure list =
  List.fold_left (
      fun (stmst, proc_acc) h -> (
      let (new_stmts, new_proc_acc) = rebuild_statement h other_procs proc_acc in 
      (stmst @ new_stmts, new_proc_acc))) 
    ([], acc) stmts

(** [rebuild_statement stmt other_procs acc] rebuilds the given statement [stmt] and updates the list of procedures based on the call graph. *)
and rebuild_statement (stmt: Ast.statement) (other_procs: Ast.procedure list) (acc: Ast.procedure list) : Ast.statement list * Ast.procedure list =
  match stmt with
  | Ast.IfStmt (c1, tb, fb, c2) ->
    (match c1 with 
    | Ast.Constant const when const <> 0 -> rebuild_statements tb other_procs acc
    | Ast.Constant const when const = 0 -> rebuild_statements fb other_procs acc
    | _ -> 
      let (tb_stmts, tb_acc) = rebuild_statements tb other_procs acc in
      let (fb_stmts, fb_acc) = rebuild_statements fb other_procs tb_acc in
      ([Ast.IfStmt (c1, tb_stmts, fb_stmts, c2)], fb_acc))
  | Ast.CallStmt id -> 
    if List.exists (fun (existing_id, _) -> existing_id = id) acc then
      ([Ast.CallStmt id], acc)
    else
      if List.exists (fun (existing_id, _) -> existing_id = id) other_procs then
        let next_proc = Procedure.find id other_procs in
        let new_acc = rebuild_proc next_proc other_procs (next_proc :: acc) in
        ([Ast.CallStmt id], new_acc)
      else
        failwith "Uncall to undefined procedure"
  | Ast.UncallStmt id ->
    if List.exists (fun (existing_id, _) -> existing_id = id) acc then
      ([Ast.UncallStmt id], acc)
    else
      if List.exists (fun (existing_id, _) -> existing_id = id) other_procs then
        let next_proc = Procedure.find id other_procs in
        let new_acc = rebuild_proc next_proc other_procs (next_proc :: acc) in
        ([Ast.UncallStmt id], new_acc)
      else
        failwith "Uncall to undefined procedure"
  | Ast.LoopStmt (c1, lb, c2) ->
    (match c1 with
    | Ast.Constant const when const = 0 -> ([], acc)
    | _ ->
      let (rebuild_loop_stmts, lb_acc) = rebuild_statements lb other_procs acc in
      ([Ast.LoopStmt (c1, rebuild_loop_stmts, c2)], lb_acc))
  | Ast.ModStmt (id, bop, e) -> ([Ast.ModStmt (id, bop, e)], acc)
  | Ast.SwapStmt (id1, id2) -> ([Ast.SwapStmt (id1, id2)], acc)
  | Ast.SkipStmt -> ([], acc)

(** [optimize prog] performs dead code elimination by removing statements that have no effect and 
    analyzing the call graph for eliminating unreachable procedures in the given program [prog]. 
    The procedures of the program are recursively traversed top down. An accumulator is used to simultaniously 
    rebuild the procedures of the program. *)
let optimize (prog : Ast.program) : Ast.program =
  let (variables, procs) = prog in
  let main_proc = Procedure.last procs in
  let kept_procs = rebuild_proc main_proc procs [main_proc] in
  (variables, kept_procs)