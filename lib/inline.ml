module Procedure = Procedure
module Invert = Invert

(* Helpers *)
(** These helper functions are used to generate a list of proc identifiers that are called by the the given [proc]*)
let rec find_calls_in_statements (stmts: Ast.statement list) (other_procs: Ast.procedure list) (acc: Ast.identifier list): Ast.identifier list =
  List.fold_left (
      fun stmts_acc s -> (
        let new_call = find_call_in_statement s other_procs stmts_acc in 
        (stmts_acc @ new_call)
      )) 
    acc stmts

and find_call_in_statement (stmt: Ast.statement) (other_procs: Ast.procedure list) (acc: Ast.identifier list): Ast.identifier list =
  match stmt with
  | Ast.CallStmt id | Ast.UncallStmt id ->
    if List.mem id acc then 
      acc
    else 
      let (_, stmts) = Procedure.find id other_procs in 
      find_calls_in_statements stmts other_procs (id :: acc) 
  | Ast.IfStmt (_, tb, fb, _) -> 
    let tb_calls_acc = find_calls_in_statements tb other_procs acc in
    let fb_calls_acc = find_calls_in_statements fb other_procs tb_calls_acc in
    fb_calls_acc
  | Ast.LoopStmt (_, lb, _) -> find_calls_in_statements lb other_procs acc
  | _ -> acc

(** Finds the procedures that are called by the given procedure.*)
let find_calls_in_proc (proc: Ast.procedure) (other_procs: Ast.procedure list): Ast.identifier list =
  let (_, stmts) = proc in
  find_calls_in_statements stmts other_procs []

(* Optimization *)

let rec inline_statements (stmts: Ast.statement list) (other_procs: Ast.procedure list): Ast.statement list = 
  List.fold_left (fun acc stmt ->
        let new_stmts = inline_statement stmt other_procs in 
        acc @ new_stmts
      ) [] stmts

and inline_statement (stmt: Ast.statement) (other_procs: Ast.procedure list) : Ast.statement list = 
  match stmt with
  | Ast.IfStmt (c1, tb, fb, c2) ->
    let tb_stmts = inline_statements tb other_procs in
    let fb_stmts = inline_statements fb other_procs in
    [Ast.IfStmt (c1, tb_stmts, fb_stmts, c2)]
  | Ast.CallStmt id -> 
    let proc = Procedure.find id other_procs in
    let (id, stmts) = proc in
    if List.mem id (find_calls_in_proc proc other_procs) then
      [Ast.CallStmt id]
    else
      inline_statements stmts other_procs
  | Ast.UncallStmt id ->
    let proc = Procedure.find id other_procs in
    let (id, _) = proc in
    if List.mem id (find_calls_in_proc proc other_procs) then
      [Ast.UncallStmt id]
    else
      let inverted_procs = List.map (fun (p) -> Invert.invert_procedure p) other_procs in
      let (_, stmts) = Invert.invert_procedure (Procedure.find id other_procs) in
      inline_statements stmts inverted_procs
  | Ast.LoopStmt (c1, lb, c2) ->
      let lb_stmts = inline_statements lb other_procs in
      [Ast.LoopStmt (c1, lb_stmts, c2)]
  | Ast.ModStmt (id, bop, e) -> [Ast.ModStmt (id, bop, e)]
  | Ast.SwapStmt (id1, id2) -> [Ast.SwapStmt (id1, id2)]
  | Ast.SkipStmt -> [Ast.SkipStmt]

let inline_proc (proc: Ast.procedure) (other_procs: Ast.procedure list) : Ast.procedure =
  let (id, stmts) = proc in 
  let new_stmts = inline_statements stmts other_procs in
  (id, new_stmts)

(** Optimization is done by seperatly inlining all procedures in the program. 
   A procedure is then inlined by inlining all the statements in the procedures. 
   Each time a call or uncall statement is being inlined the we first need to check if the called procedure is recursive. 
   This is done by first evaluting the find_calls_in_proc proc other_procs list. If the procedure is present in this list, the 
   procedure is not inlined. The call to the procedure is removed. If the removal of the call breaks the reachability of the 
   procedure from the main proc, then a seperate pass of the optimization loop from solution.ml will remove the now unreachable proc
   (deadcode) *)
let optimize (prog : Ast.program) : Ast.program =
  let (variables, procs) = prog in
  let new_procs = List.map (fun (proc) -> inline_proc proc procs) procs in
  (variables, new_procs)
