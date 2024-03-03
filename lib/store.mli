type store


(** Initializes a store with a list of identifiers, associating 
    each identifier with an initial value of 0.

    TODO: How should duplicate initialization be handled?

    Args:
        ids (Ast.identifier list): List of identifiers 
        to be initialized in the store.

    Returns:
        store: A list of tuples where each tuple consists of 
        an identifier and its associated initial value (0).
*)
val init : Ast.identifier list -> store 

(** [read id store] retrieves the value associated with the given 
    identifier [id] from the variable [store].

    Raises:
    - [Failure] if the variable with the provided [id] is not found 
    in the [store].
*)
val read : Ast.identifier -> store -> int

(** [write (id, value) store] writes the given identifier [id] with its 
    associated value [value] into the variable [store]. The identifier 
    must already exist in the store; otherwise, a [failwith] error is 
    raised.

    Args:
        (id, value) (Ast.identifier * int): Identifier-value pair 
        to be written into the store.
        store: Current variable store.

    Returns:
        store: Updated variable store.
    Raises:
        - [Failure] if the identifier [id] is not found in the store.
*)
val write : Ast.identifier -> int -> store -> store

(** [string store] returns a string representation of the variable store.

    The function takes a [store], which is a list of identifier-value pairs,
    and produces a string representation of the store in the form of a list of
    tuples. Each tuple consists of an identifier and its associated value.

    Examples:
    - If the store is empty, the result is "[]".
    - If the store contains [(x, 10); (y, 20)], the result is
      "[(x, 10); (y, 20)]".

    Args:
        store: A variable store represented as a list of identifier-value pairs.

    Returns:
        string: A string representation of the variable store.
*)
val to_string: store -> string 
