(*  Debugger Functions *)

structure Util =
struct

  (* ********************
  *  For unimplemented functions.
  *  ******************** *)
  exception Unimplemented

  val debug = false ;

  fun dbg_print (s : unit -> string) : unit =
    if debug then print (s()) else ()

  fun dbg_printnl (s : unit -> string) : unit =
    dbg_print (fn () => s() ^ "\n")

  (* Low-precedence application.
  *  Note:  clients of Util must explicitly make $ infix at precedence 0 *)
  infix 0 $
  fun f $ x = f x

end
