(* Abstract syntax for mini-langues *)
structure Ast =
struct

  type ident = string

  structure IdentKey : ORD_KEY =
  struct
    type ord_key = ident
    val compare = String.compare
  end

  (*  A pattern is something an expression is bound to---e.g., the LHS of a
  *  value declaration or a function parameter.  We only have two kinds of
  *  patterns:  identifiers and the wild pattern.
  *)
  datatype pat = PIdent of ident | PWild

  datatype binop = IPlus | IMinus | ITimes | IDiv | Mod
                 | RPlus | RMinus | RTimes | RDiv
                 | ILt | ILe | IGt | IGe 
                 | RLt | RLe | RGt | RGe
                 | Eq | Ne
                 | Cat
                 | Cons | Append

  datatype exp = Ident of ident | Int of int | Real of real
               | Char of char | Str of string | Bool of bool 
               | Triv
               | Binop of binop*exp*exp
               | Orelse of exp*exp | Andalso of exp*exp 
               | Cond of exp*exp*exp 
               | Sel of int | Tuple of exp list 
               | Nil
               | App of exp*exp | Lambda of (pat list)*exp 
               | Let of (dec list)*exp

  (*  The type of program declarations.
  *)
  and dec = ValDec of pat*exp 
          | FunDec of ident*(pat list)*exp

  (*  The type of program statements.
  *)
  datatype stm = Dec of dec
               | ExprStm of exp

  (*  The type of programs.
  *)
  datatype pgm = Program of (stm list)

  (*  patToString p = a string representation of p.
  *)
  fun patToString (p : pat) : string =
    case p of
         PIdent x => x
       | PWild => "_"

  fun binopToString (b : binop) : string =
    case b of
         IPlus => "+"
       | IMinus => "-"
       | ITimes => "*"
       | IDiv => "/"
       | Mod => "mod"
       | RPlus => "+#"
       | RMinus => "-#"
       | RTimes => "*#"
       | RDiv => "/#"
       | ILt => "<"
       | IGt => ">"
       | ILe => "≤"
       | IGe => "≥"
       | RLt => "<#"
       | RGt => ">#"
       | RLe => "≤#"
       | RGe => "≥#"
       | Eq => "="
       | Ne => "≠"
       | Cat => "^"
       | Cons => "::"
       | Append => "@"
    
  (* expToString : t -> string
  *  expToString e is the string representation of ASTs. 
  *)
  local
    fun wrap s = concat ["(", s, ")"]
  in
    fun binop b e1 e2 = 
      concat [expToString e1, binopToString b, expToString e2]
    and expToString(Ident(x)) = x
      | expToString(Int n) = Int.toString n
      | expToString(Real n) = Real.toString n
      | expToString(Char c) = Char.toString c
      | expToString(Str s) = concat ["\"", s, "\""]
      | expToString(Bool b) = Bool.toString b
      | expToString(Triv) = "(_)"
      | expToString(Binop(b, e1, e2)) =
          wrap (binop b e1 e2)
      | expToString(Orelse(e1, e2)) =
          concat ["(", expToString e1, " orelse ", expToString e2, ")"]
      | expToString(Andalso(e1, e2)) =
          concat ["(", expToString e1, " andalso ", expToString e2, ")"]
      | expToString(Cond(e1, e2, e3)) =
          String.concatWith " " 
          ["if", expToString e1, "then", expToString e2, 
                                 "else", expToString e3, "fi"]
      | expToString(Sel(n)) = 
          concat ["#", Int.toString n]
      | expToString(Tuple es) = 
          ListFormat.fmt {init="(", final=")", sep=", ", fmt=expToString} es
      | expToString(Nil) = "nil"
      | expToString(App(rator, rand)) =
          concat ["(", expToString rator, "@@", expToString rand, ")"]
      | expToString(Lambda(ps, e)) =
          concat [
            "( fn ", 
            String.concatWith " " (map patToString ps), 
            " => ", 
            expToString e, ")"
          ]
      | expToString(Let(decs, e2)) =
          concat ["( let ", ListFormat.listToString declToString decs, 
                  " in ", expToString e2, 
                  " end)"]

    (*  declToString d = s, where s is a string representation of the
    *   declaration d.
    *)
    and declToString (ValDec(p, e)) =
          String.concatWith " " ["val", patToString p, "=", expToString e]
      | declToString (FunDec(f, ps, e)) =
          String.concatWith " " [
            "fun", f, 
            String.concatWith " " (map patToString ps), "=", 
            expToString e
          ]
  end

  (*  stmToString stm = s, where s is a string representation of stm.
  *)
  fun stmToString (stm : stm) : string =
    case stm of
         Dec d => (declToString d) ^ " ;"
       | ExprStm e => (expToString e) ^ " ;"


  (*  toString pgm = s, where s is a string representation of the program pgm.
  *)
  fun pgmToString (Program(stms)) : string =
    String.concatWith "\n" (map stmToString stms)

  fun expEq(e, e') =
    case (e, e') of
         (Ident x, Ident x') => x = x'
       | (Int n, Int n') => n = n'
       | (Real x, Real x') => Real.==(x, x')
       | (Char c, Char c') => c = c'
       | (Str s, Str s') => s = s'
       | (Bool b, Bool b') => b = b'
       | (Triv, Triv) => true
       | (Binop(b, e0, e1), Binop(b', e0', e1')) =>
           b = b' andalso expEq(e0, e0') andalso expEq(e1, e1')
       | (
          (Orelse(e0, e1), Orelse(e0', e1'))
         |(Andalso(e0, e1), Andalso(e0', e1'))
         |(App(e0, e1), App(e0', e1'))
         ) =>
             expEq(e0, e0') andalso expEq(e1, e1')
       | (Cond(e, e0, e1), Cond(e', e0', e1')) =>
           expEq(e, e') andalso expEq(e0, e0') andalso expEq(e1, e1')
       | (Sel(n), Sel(n')) => n = n'
       | (Tuple es, Tuple es') => ListPair.all expEq (es, es')
       | (Nil, Nil) => true
       | (Lambda(ps, e), Lambda(ps', e')) => ps = ps' andalso expEq(e, e')
       | (Let(ds, e), Let(ds', e')) => 
           ListPair.all decEq (ds, ds') andalso expEq(e, e')
       | _ => false

  and decEq(d, d') =
    case (d, d') of
         (ValDec(x, e), ValDec(x', e')) => x = x' andalso expEq(e, e')
       | (FunDec(x, ps, e), FunDec(x', ps', e')) =>
           x = x' andalso ps = ps' andalso expEq(e, e')
       | _ => false

  fun stmEq(s, s') =
    case (s, s') of
         (Dec d, Dec d') => decEq(d, d')
       | (ExprStm e, ExprStm e') => expEq(e, e')
       | _ => false

  fun pgmEq(Program ss, Program ss') =
    ListPair.all stmEq (ss, ss')

end
