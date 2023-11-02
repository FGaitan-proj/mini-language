(*  Interpreter *)

structure Interp =
struct

  (* Debugger *)
  val dbg_print = Util.dbg_print
  val dbg_printnl = Util.dbg_printnl
  val $ = Util.$
  infix 0 $

  (* ********************
  *  Exceptions.
  *  ********************
  *)
  exception InterpError of string

  (* ********************
  *  RecClosure(f, [x_k,...,x_{n-1}], e, rho, [x_0,...,x_{k-1}])
  *  represents the (possibly) recursive function f x_0 ... x_{n-1} that has
  *  been partially applied to arguments for x_0,...,x_{k-1}.
  *
  *  Builtin id represents the built-in function id.  Each built-in function
  *  is associated to a function that maps values to values, specified by the
  *  baseEnv environment.
  *  ********************
  *)
  datatype value = Int of int | Real of real | Char of char | Str of string
                 | Bool of bool | Triv | Sel of int
                 | Tuple of value list | List of value list
                 | Lets of value Env.env
                 | FN of (Ast.pat list * Ast.exp * value Env.env)
                 | Bif of value -> value
                 | ReFN of
                    (Ast.ident * Ast.pat list *
                      Ast.exp * value Env.env * Ast.pat list)

  datatype config = Con of Ast.exp * (value Env.env) | V of value
                    |DCon of Ast.dec * (value Env.env)

  datatype stack = Lop of (Ast.binop)*Ast.exp*(value Env.env)
            | Rop of Ast.binop * value
            | Tp of (Ast.exp list)*(value Env.env)*(value list)
            | Cond of Ast.exp * Ast.exp * (value Env.env)
            | AA of Ast.exp * (value Env.env)
            | OE of Ast.exp * (value Env.env)
            | Let of (Ast.dec list) * Ast.exp
            | vald of Ast.pat * (value Env.env)
            | fund of (value Env.env)
            | App of Ast.exp * (value Env.env)
            | App' of value

  (*  valueToString v = a string representation of v.
  *)
  fun valueToString (v : value) : string =
    case v of
         Int n => Int.toString n
       | Real x => Real.toString x
       | Char c => "#\"" ^ Char.toString c ^ "\""
       | Str s => "\"" ^ String.toString s ^ "\""
       | Bool b => Bool.toString b
       | Triv => "()"
       | Tuple vs =>
           ListFormat.fmt {init="(", final=")", sep=",", fmt=valueToString} vs
       | List vs =>
           ListFormat.listToString valueToString vs
       | FN(pl,e,v) => Ast.expToString(Ast.Lambda(pl, e))
       | Sel i => Ast.expToString(Ast.Sel(i))
       | _ => "Other value"

  (*veq v v' = bool where true if v = v' and false otherwise
  *)
  fun veq(v: value)(v': value): bool =
      case (v,v') of
       (Int n, Int n') => n = n'
      | (Real x, Real x') => Real.==(x, x')
      | (Char c, Char c') => c = c'
      | (Str s, Str s') => s = s'
      | (Bool b, Bool b') => b = b'
      | (Triv, Triv) => true
      | (List([]),List([])) => true
      |_ => raise InterpError "values cannot be compared"

  (*vlseq [v_0,..] [v_0',...] = bool where true if
  * v_i = v_i' and false otherwise
  *)
  fun vlseq(v0 : value list)(v1 : value list): bool =
      case (v0,v1) of
        ([],[]) => true
        |(v::vs0, v'::vs1) => (veq v v') andalso (vlseq vs0 vs1)
        |_ => false

  (*****Transition function*****)
  fun trans (cfg: config) (k: stack list) : config * (stack list) =
  case (cfg,k) of
    (Con(Ast.Int n, _), _) => (V (Int n), k)
    |(Con(Ast.Real x, _), _) => (V (Real x), k)
    |(Con(Ast.Str s, _), _) => (V(Str s), k)
    |(Con(Ast.Char c, _), _) => (V(Char c), k)
    |(Con(Ast.Bool b, _), _) => (V(Bool b), k)
    |(Con(Ast.Sel i, _), _) => (V(Sel i), k)
    |(Con(Ast.Triv, _), _) => (V(Triv), k)
    |(Con(Ast.Nil, _), _) => (V(List []), k)
    |(Con(Ast.Lambda(pl,e0),rho),_) => (V(FN(pl,e0,rho)), k)
    |(Con(Ast.Ident x, rho), _) => (V(Env.get rho x), k)
    |(Con(Ast.Tuple(e::es), rho), k) =>
        (Con(e,rho), Tp(es, rho, [])::k)
    |(V m, Tp([],rho,ms)::k) => (V (Tuple (ms@[m])), k)
    |(V m, Tp(e::es, rho, ms)::k) =>
        (Con(e,rho), Tp(es,rho,ms@[m])::k)
    |(Con(Ast.Binop(b,e0,e1), env), _) =>
        (Con(e0,env), Lop(b, e1, env) :: k)
    |(V m, Lop(b,e1,env) :: k) => (Con(e1, env), Rop(b,m) :: k)
    |(V (Int n), Rop(Ast.IPlus, Int m) :: k) => (V(Int (m + n)),k)
    |(V (Int n), Rop(Ast.IMinus, Int m) :: k) => (V(Int (m - n)),k)
    |(V (Int n), Rop(Ast.ITimes, Int m) :: k) => (V(Int (m * n)),k)
    |(V (Int n), Rop(Ast.IDiv, Int m) :: k) => (V(Int (m div n)),k)
    |(V (Int n), Rop(Ast.Mod, Int m) :: k) => (V(Int (m mod n)),k)
    |(V (Int n), Rop(Ast.ILt, Int m) :: k) => (V(Bool (m < n)),k)
    |(V (Int n), Rop(Ast.ILe, Int m) :: k) => (V(Bool (m <= n)),k)
    |(V (Int n), Rop(Ast.IGt, Int m) :: k) => (V(Bool (m > n)),k)
    |(V (Int n), Rop(Ast.IGe, Int m) :: k) => (V(Bool (m >= n)),k)
    |(V (Int n), Rop(Ast.Eq, Int m) :: k) => (V(Bool (m = n)),k)
    |(V (Int n), Rop(Ast.Ne, Int m) :: k) => (V(Bool (m <> n)),k)
    |(V (Real n), Rop(Ast.RPlus, Real m) :: k) => (V(Real (m + n)),k)
    |(V (Real n), Rop(Ast.RMinus, Real m) :: k) => (V(Real (m - n)),k)
    |(V (Real n), Rop(Ast.RTimes, Real m) :: k) => (V(Real (m * n)),k)
    |(V (Real n), Rop(Ast.RDiv, Real m) :: k) => (V(Real (m/n)),k)
    |(V (Real n), Rop(Ast.RLt, Real m) :: k) => (V(Bool (m < n)),k)
    |(V (Real n), Rop(Ast.RLe, Real m) :: k) => (V(Bool (m <= n)),k)
    |(V (Real n), Rop(Ast.RGt, Real m) :: k) => (V(Bool (m > n)),k)
    |(V (Real n), Rop(Ast.RGe, Real m) :: k) => (V(Bool (m >= n)),k)
    |(V (Bool n), Rop(Ast.Eq, Bool m) :: k) => (V(Bool (m = n)),k)
    |(V (Bool n), Rop(Ast.Ne, Bool m) :: k) => (V(Bool (m <> n)),k)
    |(V (Str n), Rop(Ast.Cat, Str m) :: k) => (V(Str (m^n)),k)
    |(V (Char n), Rop(Ast.Eq, Char m) :: k) => (V(Bool (m = n)),k)
    |(V (Char n), Rop(Ast.Ne, Char m) :: k) => (V(Bool (m <> n)),k)
    |(V (List n), Rop(Ast.Cons, m) :: k) => (V(List (m::n)),k)
    |(V (List n), Rop(Ast.Append, List m) :: k) => (V(List (m@n)),k)
    |(V (List n), Rop(Ast.Ne, List m) :: k) => (V(Bool(not(vlseq m n))),k)
    |(V (List n), Rop(Ast.Eq, List m) :: k) => (V(Bool(vlseq m n)),k)
    |(Con(Ast.Cond(e,e0,e1),rho), k) => (Con(e,rho),Cond(e0,e1,rho)::k)
    |(V (Bool true), Cond(e0,e1,rho)::k) => (Con(e0,rho),k)
    |(V (Bool false), Cond(e0,e1,rho)::k) => (Con(e1,rho), k)
    |(Con(Ast.Andalso(e0,e1),rho), k) => (Con(e0,rho),AA(e1,rho)::k)
    |(V (Bool false), AA(e1,rho)::k) => (V(Bool false), k)
    |(V (Bool true), AA(e1,rho)::k) => (Con(e1,rho), k)
    |(Con(Ast.Orelse(e0,e1),rho), k) => (Con(e0,rho),OE(e1,rho)::k)
    |(V (Bool true), OE(e1,rho)::k) => (V(Bool true), k)
    |(V (Bool false), OE(e1,rho)::k) => (Con(e1,rho), k)
    |(Con(Ast.Let(d::ds,e0),rho), k) => (DCon(d,rho), Let(ds,e0)::k)
    |(DCon(Ast.ValDec(x,e),rho),k) => (Con(e,rho), vald(x,rho) :: k)
    |(V m, vald (Ast.PIdent x, rho) :: k) =>
        (V (Lets(Env.update rho x m)), k)
    |(V m, vald (Ast.PWild, rho) :: k) => (V (Lets rho), k)
    |(DCon(Ast.FunDec(x,pl,e),rho),k) =>
        (V (Lets(Env.update rho x (ReFN(x,pl,e,rho,pl)))), k)
    |(V (Lets rho'),Let(d::ds,e0)::k) =>  (DCon(d,rho'), Let(ds,e0)::k)
    |(V (Lets rho'),Let([],e0)::k) => (Con(e0,rho'),k)
    |(Con(Ast.App(e,e0), rho), k) => (Con(e0,rho), App(e,rho)::k)
    |(V m, App(e,rho)::k) => (Con(e,rho), (App' m)::k)
    |(V (FN([Ast.PWild],e,rho)), (App' m)::k) => (Con(e,rho), k)
    |(V (FN([Ast.PIdent x],e,rho)), (App' m)::k) =>
        (Con(e,(Env.update rho x m)), k)
    |(V (FN((Ast.PWild)::p,e,rho)), (App' m)::k) =>
        (V (FN(p,e,rho)), k)
    |(V (FN((Ast.PIdent x)::p,e,rho)), (App' m)::k) =>
        (V (FN(p,e,(Env.update rho x m))), k)
    |(V (ReFN(x,[Ast.PWild],e,rho,pl')), (App' m)::k) => (Con(e,rho), k)
    |(V (ReFN(x,[Ast.PIdent y],e,rho,pl')), (App' m)::k) =>
        let
          val env = Env.updateMany rho
                    [(y,m),(x, ReFN(x,pl',e,rho,pl'))]
        in (Con(e,env), k) end
    |(V (ReFN(x,(Ast.PWild)::ps,e,rho,pl')), (App' m)::k) =>
        (V (ReFN(x,ps,e,rho, pl')), k)
    |(V (ReFN(x,(Ast.PIdent y)::ps,e,rho,pl')), (App' m)::k) =>
        (V (ReFN(x,ps,e, (Env.update rho y m), pl')), k)
    |(V (Bif vfun), (App' m)::k) => (V (vfun m), k)
    |(V (Sel i), (App' (Tuple v))::k) => (V (List.nth(v,i-1)), k)
    |_ => raise InterpError "Case Match error in trans function"

(******************Evaluator Function***************************)
(*  evalE e rho = v, where rho |- e ↓ v.
*)
  fun ition(c: config) (s: stack list): value =
      case (trans c s) of
        (V k, []) => k
        |(c, s) => (ition c s)

  fun evalE (e : Ast.exp) (rho : value Env.env) : value =
      ition (Con(e,rho)) []

  (* BIfun [rho] takes some env and updates rho with the Builtin functions
  * specified in the assignment.
  *)
  fun BIfun(rho: value Env.env) : value Env.env =
      let
        fun Pr(Str s : value): value =
            (let
              val _ = print(s^"\n")
              in Str s end)
        val pI = ("printInt", Bif(fn Int n => Pr(Str (Int.toString n))))
        val pR = ("printReal", Bif(fn Real x => Pr(Str (Real.toString x))))
        val pB = ("printBool", Bif(fn Bool b => Pr(Str (Bool.toString b))))
        val pC = ("printChar", Bif(fn Char c => Pr(Str (Char.toString c))))
        val pS = ("printString", Bif(fn x => Pr(x)))
        val hd = ("hd", Bif(fn List(v::vs) => v))
        val tl = ("tl", Bif(fn List(v::vs) => List(vs)))
        val nl = ("null", Bif(fn List v => Bool (List.null v)))
        val lg = ("length",Bif(fn List v => Int (List.length v)))
        val nt = ("not", Bif(fn Bool b => Bool (not b)))
        val rl = ("real", Bif(fn Int n => Real (Real.fromInt n)))
        val ex = ("explode", Bif(fn Str s => List(
             List.map (fn c => Char c) (String.explode s)
            )))
        val im = ("implode", Bif(fn List v => Str (
             String.implode (List.map (fn Char c => c) v)
            )))
        val ch = ("chr", Bif(fn Int n => Char (Char.chr n)))
        val or = ("ord", Bif(fn Char c => Int (Char.ord c)))
        val ls = [pI,pR,pB,pC,pS,hd,tl,nl,lg,nt,rl,ex,im,ch,or]
      in
        Env.updateMany rho ls
      end
      handle Match => raise InterpError "Built-in functions"

  (* evalExp e = v where rho |- e0 ↓ v' with an empty rho *)
   fun evalExp (e : Ast.exp) : value = evalE e Env.empty

  (* execStates [s_1,...] rho = [e0,...] rho' where rho' is updated with
  * all declarations in [s_1,...] and [e0,...] are all the expressions
  * from [s_1,...].
  *)
  fun execStates (stm : Ast.stm list) (rho: value Env.env):
      Ast.exp list * value Env.env =
        case stm of
          [] => ([],rho)
          |st::ms =>
            (case st of
              Ast.ExprStm(e) =>
              let
                val (es,rho0) = execStates ms rho
              in
                (e::es,rho0)
              end
              |Ast.Dec(d) =>
              (case d of
                  Ast.ValDec(Ast.PIdent(x),e0) =>
                      execStates ms (Env.update rho x (evalE e0 rho))
                 |Ast.ValDec(Ast.PWild,e0) =>
                      let val _ = (evalE e0 rho) in execStates ms rho end
                 |Ast.FunDec(x,pl,e0) =>
                      execStates ms (Env.update rho x (ReFN(x,pl,e0,rho,pl)))
              ))

  (* execStms [e_0,...] rho = ()
  * Prints the values v_i where rho |- e_i ↓ v_i
  *)
  fun execStms (exl : Ast.exp list, rho: value Env.env): unit =
      (case exl of
        [] => ()
        |e::es =>
            let val _ = (print (valueToString (evalE e rho) ^ "\n"))
                val _ = execStms(es,rho)
            in () end
            )

  (*  execPgm p = ().
  *
  *  Effect:  the program p is executed under the base environment.  Each
  *  statement in p is executed in order.  A value declaration is executed by
  *  evaluating its RHS and then adding the binding to the current
  *  environment.  A function declaration is executed by adding the function
  *  binding to the current environment as an unapplied RecClosure.  An
  *  expression statement is executed by evaluating the expression to a value
  *  v, then printing a string representation of v to the terminal using
  *  valueToString.
  *)
  fun execPgm (Ast.Program stms : Ast.pgm) : unit =
      execStms(execStates stms (BIfun (Env.empty)))

end