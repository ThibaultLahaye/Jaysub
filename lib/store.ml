type store   = (Ast.identifier * int) list

(** Initialize Store

   [init ids] initializes a store with default values (0) for a given list of identifiers [ids].
   As there is no state in OCaml, this function returns a new store created based on the given list of identifiers.

*)
let init (ids: Ast.identifier list) : store = 
    List.map (fun id -> (id, 0)) ids 

(** Read Value from Store

  [read id store] retrieves the value associated with the given identifier [id] from the store.
  
*)
let read (id : Ast.identifier) (store : store) : int =
    try 
        List.assoc id store 
    with 
    | Not_found -> failwith ("Variable not found: " ^ id)

(** Write Value to Store

   [write id value store] updates the value associated with the given identifier [id] in the store.
   As there is no state in OCaml, this function returns a new store with the updated value.

*)
let rec write (id: Ast.identifier) (value: int) (store : store) : store =
    match store with
    | [] -> failwith ("Variable not found: " ^ id)
    | (current_id, current_value) :: tl ->
        if current_id = id then
            (id, value) :: tl
        else
            (current_id, current_value) :: write id value tl

(** Convert Store to String

   [to_string store] converts a store to its string representation for debugging or display purposes.
   Used by the REPL.

*)
let to_string (store : store) : string =
    match store with
    | [] -> "[]"
    | _ ->
        let string_of_binding (id, value) = "(" ^ id ^ ", " ^ string_of_int value ^ ")" in
        "[" ^ String.concat "; " (List.map string_of_binding store) ^ "]"
