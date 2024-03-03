open Store

module Procecure = Procedure
module Experssion = Expression
module Invert = Invert
module Fold = Fold

type program = Ast.program
type store = Store.store

let parse    = Parser.program Lexer.token (* Do not change *)

(* let feval      = fun _ -> failwith "implement me!" 1 *)
let feval (prog : Ast.program) : store =
    let (variables, procs) = prog in
    let initial_store  = init variables in 
    let main_proc = Procedure.last procs in
    Procedure.feval initial_store main_proc procs

(* *)

(* let beval      = fun _ -> failwith "implement me! 2" *)
let beval (prog : Ast.program) : store =
    let (variables, procs) = prog in
    let initial_store  = init variables in 
    let main_proc = Procedure.last procs in
    Procedure.beval initial_store main_proc procs

(* *)
(* let invert     = fun _ -> failwith "implement me! 3" *)
let invert (prog : program) : program =
    let (ids, procs) = prog in
    let inverted_procs = List.map Invert.invert_procedure procs in
    (ids, inverted_procs)

 
(* *)
let rec optimize (program : Ast.program): Ast.program = 
  let const_folded_program = Fold.program program in
  let dead_code_eliminated_program = DeadCode.optimize const_folded_program in
  let inlined_program = Inline.optimize dead_code_eliminated_program in
  if program = inlined_program then
      program
  else
      optimize inlined_program

let string_of_program = Ast.string_of_program (* Do not change *)

(* let string_of_store   = fun _ -> failwith "implement me! 8" *)
let string_of_store (store : store) : string = Store.to_string store
