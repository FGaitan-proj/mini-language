(* Env *)

structure Env =
struct

  structure M = SplayMapFn(Ast.IdentKey)

  exception Domain of Ast.ident

  (*  maps identifiers to values of type 'a *)
  type 'a env = 'a M.map

  (*  the empty environment *)
  val empty : 'a env = M.empty

  (*  update ρ x v = ρ{x |-> v} *)
  fun update (rho : 'a env) (x : Ast.ident) (v : 'a) : 'a env =
    M.insert(rho, x, v)

  (*  updateMany ρ [(id_0,v_0),...,(id_{n-1}, v_{n-1})] =
  *     ρ{id_0 |-> v_0}{id_1 |-> v_1}...{id_{n-1} |-> v_{n-1}} *)
  fun updateMany (rho : 'a env) (kvs : (Ast.ident*'a) list) : 'a env =
    foldl M.insert' rho kvs

  (*  get ρ x = ρ(x) *)
  fun get (rho : 'a env) (x : Ast.ident) =
    valOf (M.find(rho, x))
    handle Option => raise Domain x

  (*  listItemsi ρ = [(id_0,v_0),...,(id_{n-1},v_{n-1})], where
  *  - dom ρ = {id_0,...,id_{n-1}}
  *  - for 0 ≤ i < n, ρ(id_i) = v_i *)
  fun listItemsi (rho : 'a env) : (Ast.ident*'a) list =
    M.listItemsi rho

end
