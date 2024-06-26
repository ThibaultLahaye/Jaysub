(** An identifier is a name to reference a global variable. *)
type identifier = string [@@deriving show, eq]

type binop = | Plus | Min | NEq | Eq | Mult | Div [@@deriving show, eq]

(** Expressions either reference an existing global variable,
    contain an integer constant,
    or are a binary operation between two expressions. *)
type expression =
  | Ident of identifier
  | Constant of int
  | Binop of expression * binop * expression
  [@@deriving show, eq]

(** Statements are one of: a conditional if; a procedure call or uncall; a variable modification;
    a swap between two variables and a skip *)
type statement =
  | IfStmt of expression * statement list * statement list * expression
  | CallStmt of identifier
  | UncallStmt of identifier
  | ModStmt of identifier * binop * expression
  | SwapStmt of identifier * identifier
  | SkipStmt
  | LoopStmt of expression * statement list * expression
  [@@deriving show, eq]

(** A procedure has an identifier and a body, which a list of statements *)
type procedure = identifier * statement list [@@deriving show, eq]


(** A program is an optional list of declarations followed by a nonempty list of procedures *)
type program = identifier list * procedure list [@@deriving show, eq]

(* The remainder of this file gives you a number of string_of functions to convert ASTs
   Back into strings. They can be useful for inspecting whether your code is doing what it is
   supposed to do.
   For comparing ASTs in a test, use the derived show and eq.
   They are automatically generated by ppx_deriving (a preprocessor) 
   due to the [@@deriving show, eq] annotations next to the data type declarations. 
   
   Specifically, ppx_deriving creates for every annotated type XXX the following:
   a function Ast.show_XXX, Ast.equal_XXX, and a function Ast.pp_XXX.

   To use them in a test, e.g. to compare equality of two binary operators, the following suffices:
            Ast.equal_binop Ast.Plus Ast.Min
   To convert an AST node such as a procedure to a string, invoke
            Ast.show_procedure ("main", [CallStmt "other_proc"])

   The functions below do something similar, but instead of printing an AST,
   these "string_of_XXX" functions output valid JaySub syntax as strings.
   For outputting a string of JaySub procedure syntax, use:
            Ast.string_of_procedure ("main", [CallStmt "other_proc"])
   FYI: These are the same functions the CLI uses to print
   the inverted/optimized program back on stdout. *)

let rec s i = if i = 0 then "" else " " ^ s (i-1)

let string_of_bop = function
  | Plus -> "+"
  | Min -> "-"
  | Eq -> "=="
  | NEq -> "!="
  | Mult -> "*"
  | Div -> "/"

let rec string_of_expression = function
  | Ident i -> i
  | Constant i -> string_of_int i
  | Binop (e1, bop, e2) -> String.concat " " [
      string_of_expression e1 ;
      string_of_bop bop       ;
      string_of_expression e2 ]

let rec string_of_statements i = function
  | [] -> ""
  | stmt :: stmts ->
      s i ^ string_of_statement i stmt ^ "\n" ^
      string_of_statements i stmts

and string_of_statement i = function
  | IfStmt (c, t, e, a) ->
    "IF (" ^ string_of_expression c ^ ")\n" ^
    s i ^ "THEN {" ^ "\n" ^
    string_of_statements (i+2) t ^
    s i ^ "} ELSE {" ^ "\n" ^
    string_of_statements (i+2) e ^
    s i ^ "} FI (" ^ string_of_expression a ^ ")"
  | CallStmt id -> "CALL " ^ id ^ ";"
  | UncallStmt id -> "UNCALL " ^ id ^ ";"
  | ModStmt (id, bop, e) ->
    id ^ " " ^ string_of_bop bop ^ "= " ^ string_of_expression e ^ ";"
  | SwapStmt (id1, id2) ->
    id1 ^ " <=> " ^ id2 ^ ";"
  | SkipStmt -> "SKIP;"
  | LoopStmt (t, lb, a) ->
    "FROM (" ^ string_of_expression t ^ ")\n" ^
    s i ^ "DO {" ^ "\n" ^
    string_of_statements (i+2) lb ^
    s i ^ "} UNTIL (" ^ string_of_expression a ^ ")"


let string_of_procedure i (id, stmts) =
  s i ^ "PROCEDURE " ^ id ^ " {\n" ^
  string_of_statements (i + 2) stmts ^ "}"
let string_of_program ((ids, procs) : program) =
  String.concat ", " ids ^ ";\n\n" ^
  (List.map (string_of_procedure 0) procs
  |> String.concat "\n\n") ^ "\n"
