(** [find id procs] finds and returns the procedure with the specified
    identifier [id] from the list of procedures [procs].

    Raises:
    - [Failure] if the procedure with the provided [id] is not found 
      in the list of procedures.

    Args:
        id: Identifier of the procedure to be found.
        procs: List of procedures to search.

    Returns:
        Ast.procedure: The procedure with the specified identifier.
*)
val find : Ast.identifier -> Ast.procedure list -> Ast.procedure

(** [last procs] returns the last procedure in the list of procedures [procs].

    Raises:
    - [Failure] if the list of procedures is empty.

    Args:
        procs: List of procedures.

    Returns:
        Ast.procedure: The last procedure in the list.
*)
val last : Ast.procedure list -> Ast.procedure

val feval : Store.store -> Ast.procedure -> Ast.procedure list -> Store.store

val beval : Store.store -> Ast.procedure -> Ast.procedure list -> Store.store
