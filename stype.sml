(* Type-Checker *)

structure SType =
struct

  open Env
  infix 0 $

  (*  Raised for type-checking errors.
  *)
  exception TypeError of string

  structure TyVar =
  struct
    type tyvar = int

    val next : int ref = ref 0

    (*  toString tv = a string representation of tv.
    *)
    fun toString (sub : tyvar) : string =
    let
      val tvname = "a"
    in
      String.concat [tvname, Int.toString sub]
    end

    fun fresh() : tyvar = !next before (next := !next + 1)

  end

  type tyvar = TyVar.tyvar

  (*  Object language types and type schemes.
  *)
  datatype typ = Var of tyvar
               | IntTy | RealTy | StrTy | CharTy | BoolTy | UnitTy
               | ProdTy of typ list
               | ListTy of typ
               | ArrowTy of typ*typ

  type scheme = tyvar list * typ

  (*  typeToString t = a string representation of t.
  *)
  fun typeToString (t : typ) : string =
    case t of
         Var tv => "'" ^ TyVar.toString tv
       | IntTy => "int"
       | RealTy => "real"
       | StrTy => "str"
       | CharTy => "char"
       | BoolTy => "bool"
       | UnitTy => "unit"
       | ProdTy ts =>
           ListFormat.fmt {
             init="", final="", sep=" * ", fmt=typeToString
           } ts
       | ListTy t => typeToString t ^ " list"
       | ArrowTy(t0, t1) => 
           "(" ^ typeToString t0 ^ ") -> (" ^ typeToString t1 ^ ")"

  fun schemeToString ((vs, sigma) : scheme) : string =
    concat [
      "∀",
      ListFormat.listToString TyVar.toString vs,
      ".",
      typeToString sigma
    ]

  (*  The base type environment.
  *)
  val builtins : scheme Env.env = Env.updateMany Env.empty [
      ("printInt", ([], ArrowTy(IntTy, UnitTy)))
    , ("printReal", ([], ArrowTy(RealTy, UnitTy)))
    , ("printChar", ([], ArrowTy(CharTy, UnitTy)))
    , ("printString", ([], ArrowTy(StrTy, UnitTy)))
    , ("printBool", ([], ArrowTy(BoolTy, UnitTy)))
    , ("not", ([], ArrowTy(BoolTy, BoolTy)))
    , ("real", ([], ArrowTy(IntTy, RealTy)))
    , ("explode", ([], ArrowTy(StrTy, ListTy CharTy)))
    , ("implode", ([], ArrowTy(ListTy CharTy, StrTy)))
    , ("chr", ([], ArrowTy(IntTy, CharTy)))
    , ("ord", ([], ArrowTy(CharTy, IntTy)))
    , ("hd", ([0], ArrowTy(ListTy $ Var 0, Var 0)))
    , ("tl", ([0], ArrowTy(ListTy $ Var 0, ListTy $ Var 0)))
    , ("null", ([0], ArrowTy(ListTy $ Var 0, BoolTy)))
    , ("length", ([0], ArrowTy(ListTy $ Var 0, IntTy)))
  ]

  (*  ********************
  *   Type substitutions
  *
  *   A type substitution is a list [n₀ -> τ₀,...] of bindings of type
  *   variables to types.  A type substitution is applied to a type
  *   left-to-right, so a later binding may substitute for a type variable
  *   introduced by an earlier binding.
  *   ********************
  *)

  datatype binding = --> of tyvar*typ
  infix 1 -->
  type tysub = binding list

  (*  apply1 [α -> τ] σ = σ[α -> τ].
  *)
  fun apply1 (b as n --> tau : binding) (sigma : typ) : typ =
    case sigma of
         Var m => if m = n then tau else sigma
       | (IntTy | RealTy | StrTy | CharTy | BoolTy | UnitTy) => sigma
       | ProdTy tys => ProdTy (map $ apply1 b $ tys)
       | ListTy ty => ListTy (apply1 b ty)
       | ArrowTy(rho, rho') => ArrowTy(apply1 b rho, apply1 b rho')

  (*  apply [α₀ → τ₀, α₁ → τ₁, ...] σ = σ[α₀ → τ₀][α₁ → τ₁]...
  *)
  fun apply (xi : tysub) (sigma : typ) : typ =
    case xi of
         [] => sigma
       | b :: xi => apply xi (apply1 b sigma)

  (*  instantiateFresh ∀α₀α₁....σ = σ[α₀ → β₀, α₁ → β₁, ...], where
  *  β₀, β₁, etc. are fresh type variables.
  *)
  fun instantiateFresh ((vs, s) : scheme) : typ =
    apply (map (fn v => v --> (Var $ TyVar.fresh())) vs) s



  (*  Annotated expressions.  An annotated expression is an expression along
  *  with a type.
  *)
  type ident = Ast.ident
  type   pat = Ast.pat * typ

  (*
  datatype binop = IPlus | IMinus | ITimes | IDiv | Mod
                 | RPlus | RMinus | RTimes | RDiv
                 | ILt | ILe | IGt | IGe 
                 | RLt | RLe | RGt | RGe
                 | Eq | Ne
                 | Cat
                 | Cons | Append
  *)

  datatype annexp = AnnExp of exp * typ
  and      exp    = Ident of ident
                  | Int of int
                  | Real of real
                  | Char of char
                  | Str of string
                  | Bool of bool
                  | Triv
                  | Binop of Ast.binop*annexp*annexp
                  | Orelse of annexp*annexp
                  | Andalso of annexp*annexp
                  | Cond of annexp*annexp*annexp
                  | Sel of int
                  | Tuple of annexp list
                  | Nil
                  | App of annexp*annexp
                  | Lambda of (pat list)*annexp
                  | Let of (dec list)*annexp
  and      dec    = ValDec of pat*annexp
                  | FunDec of ident*typ*(pat list)*annexp

  datatype stm = Dec of dec
               | ExprStm of annexp

  datatype pgm = Program of stm list

  fun typeOf (AnnExp(_, ty) : annexp) : typ =
    ty

  fun applyPat (xi : tysub) (p : pat) : pat =
    case p of
         (Ast.PIdent x, sigma) => (Ast.PIdent x, apply xi sigma)
       | (Ast.PWild, sigma) => (Ast.PWild, apply xi sigma)

  val applyPats : tysub -> pat list -> pat list =
    fn xi => map $ applyPat xi

  fun applyAnnExp (xi : tysub) (AnnExp(e, sigma) : annexp) : annexp =
  let
    val apply' = applyAnnExp xi
    val sigma' = apply xi sigma
  in
    case e of
         (
             Ident _
           | Int _ | Real _ | Char _ | Str _ | Bool _ | Triv
           | Sel _ | Nil
         ) => AnnExp(e, sigma')
       | Binop(b, e0, e1) => 
           AnnExp( Binop(b, apply' e0, apply' e1), sigma')
       | Orelse(e0, e1) =>
           AnnExp( Orelse(apply' e0, apply' e1), sigma')
       | Andalso(e0, e1) =>
           AnnExp( Andalso(apply' e0, apply' e1), sigma')
       | Cond(e, e0, e1) =>
           AnnExp(Cond(apply' e, apply' e0, apply' e1), sigma')
       | Tuple es =>
           AnnExp(Tuple (map apply' es), sigma')
       | App(e0, e1) =>
           AnnExp(App(apply' e0, apply' e1), sigma')
       | Lambda(ps, e) =>
           AnnExp $ (Lambda(applyPats xi ps, applyAnnExp xi e), sigma')
       | Let(ds, e) => 
           AnnExp $ (Let(applyAnnDecs xi ds, applyAnnExp xi e), sigma')
  end

  and applyAnnDec (xi : tysub) (d : dec) : dec =
    case d of
         ValDec((Ast.PIdent x, sigma), e) =>
           ValDec((Ast.PIdent x, apply xi sigma), applyAnnExp xi e)
       | ValDec((Ast.PWild, sigma), e) =>
           ValDec((Ast.PWild, apply xi sigma), applyAnnExp xi e)
       | FunDec(f, sigma, ps, e) =>
           FunDec(f, apply xi sigma, applyPats xi ps, applyAnnExp xi e)

  and applyAnnDecs (xi : tysub) (ds : dec list) : dec list =
    map $ applyAnnDec xi $ ds

  fun applyAnnStm (xi : tysub) (stm : stm) : stm =
    case stm of
         Dec d => Dec(applyAnnDec xi d)
       | ExprStm e => ExprStm(applyAnnExp xi e)

  val applyAnnStms : tysub -> stm list -> stm list = 
    fn xi => map $ applyAnnStm xi

  fun getIdAnns(AnnExp e : annexp) : (Ast.ident*typ) list =
    case e of
         (  (Binop(_, e0, e1)
          | Orelse(e0, e1)
          | Andalso(e0, e1)
          | App(e0, e1)) , _) => getIdAnns e0 @ getIdAnns e1
       | (Cond(e, e0, e1), _) => getIdAnns e @ getIdAnns e0 @ getIdAnns e1
       | (Tuple es, _) => List.concat (map getIdAnns es)
       | (Lambda(ps, e), _) =>
           foldr
             (fn ((Ast.PIdent x, sigma), r) => (x, sigma) :: r
               | (_, r) => r)
             []
             ps
           @ getIdAnns e
       | (Let([], e), _) => getIdAnns e
       | (Let(ValDec((Ast.PIdent x, sigma), e') :: ds, e), tau) =>
           (x, sigma) :: getIdAnns e' @ getIdAnns(AnnExp(Let(ds, e), tau))
       | (Let(FunDec(f, fty, ps, e') :: ds, e), tau) =>
           (f, fty) ::
           foldr
             (fn ((Ast.PIdent x, sigma), r) => (x, sigma) :: r
               | (_, r) => r)
             []
             ps
           @ getIdAnns e
           @ getIdAnns(AnnExp(Let(ds, e), tau))
       | _ => []


  (*  ********************
  *   Annotation functions.
  *   ********************
  *)

  (*  annotatePat p = (p : α), where α is a fresh type variable.
  *)
  fun annotatePat (p : Ast.pat) : pat =
    case p of
         Ast.PIdent x => (Ast.PIdent x, Var $ TyVar.fresh())
       | Ast.PWild => (Ast.PWild, Var $ TyVar.fresh())

  (*  annotate η e = the annotated version of e relative to the type
  *  environment η.
  *)
  fun annotate (eta : typ Env.env) (e : Ast.exp) : annexp =
  let
    val ann = annotate eta
  in
    case e of
         Ast.Ident x => 
         (
           AnnExp $ (Ident x, Env.get eta x)
           handle Env.Domain _ =>
           (
             AnnExp (Ident x, instantiateFresh $ Env.get builtins x)
             handle Env.Domain _ =>
               raise TypeError $ "Unbound identifier:  " ^ x
           )
         )
       | Ast.Int n => AnnExp $ (Int n, IntTy)
       | Ast.Real x => AnnExp $ (Real x, RealTy)
       | Ast.Char c => AnnExp $ (Char c, CharTy)
       | Ast.Str s => AnnExp $ (Str s, StrTy)
       | Ast.Bool b => AnnExp $ (Bool b, BoolTy)
       | Ast.Triv => AnnExp $ (Triv, UnitTy)
       | Ast.Binop(
           b as (Ast.IPlus | Ast.IMinus | Ast.ITimes | Ast.IDiv | Ast.Mod), 
           e0, e1
         ) => AnnExp $ (Binop(b, ann e0, ann e1), IntTy)
       | Ast.Binop(
           b as (Ast.RPlus | Ast.RMinus | Ast.RTimes | Ast.RDiv), e0, e1
         ) => AnnExp $ (Binop(b, ann e0, ann e1), RealTy)
       | Ast.Binop(b as (Ast.ILt | Ast.IGt | Ast.ILe | Ast.IGe), e0, e1) =>
           AnnExp $ (Binop(b, ann e0, ann e1), BoolTy)
       | Ast.Binop(b as (Ast.RLt | Ast.RGt | Ast.RLe | Ast.RGe), e0, e1) =>
           AnnExp $ (Binop(b, ann e0, ann e1), BoolTy)
       | Ast.Binop(b as (Ast.Eq | Ast.Ne), e0, e1) =>
           AnnExp $ (Binop(b, ann e0, ann e1), BoolTy)
       | Ast.Binop(Ast.Cat, e0, e1) =>
           AnnExp $ (Binop(Ast.Cat, ann e0, ann e1), StrTy)
       (*  Append and Cons are polymorphic functions, so they have to be
       *  instantiated at fresh list types.
       *)
       | Ast.Binop(Ast.Append, e0, e1) =>
           let 
             val e1' = ann e1
           in
             AnnExp (Binop(Ast.Append, ann e0, e1'), 
                     ListTy(Var $ TyVar.fresh()))
           end
       | Ast.Binop(Ast.Cons, e0, e1) =>
           let 
             val e1' = ann e1
           in
             AnnExp (Binop(Ast.Cons, ann e0, e1'),
                     ListTy(Var $ TyVar.fresh()))
           end
       | Ast.Orelse(e0, e1) => 
           AnnExp (Orelse(annotate eta e0, annotate eta e1), BoolTy)
       | Ast.Andalso(e0, e1) => 
           AnnExp (Andalso(annotate eta e0, annotate eta e1), BoolTy)
       | Ast.Cond(e, e0, e1) =>
           AnnExp (Cond(ann e, ann e0, ann e1), Var $ TyVar.fresh())
       | Ast.Nil => AnnExp(Nil, ListTy $ Var (TyVar.fresh()))
       | Ast.App(e0, e1) =>
           AnnExp (App(ann e0, ann e1), Var $ TyVar.fresh())
       | Ast.Lambda(ps, e) =>
           let
             val (ps', e', resultTy) = annotateFunction eta ps e
           in
             AnnExp (Lambda(ps', e'), resultTy)
           end
       | Ast.Let(ds, e) =>
         let
           val (ds', eta') = annotateDecs eta ds
           val e' = annotate eta' e
         in
           AnnExp (Let(ds', e'), typeOf e')
         end

       | Ast.Sel _ =>
           raise Fail "Ast.Sel not supported for type inference."
       | Ast.Tuple es =>
           let
             val es' = map ann es
             val tys = map (fn (AnnExp(_, ty)) => ty) es'
           in
             AnnExp (Tuple es', ProdTy tys)
           end
  end

  (*  annotateFunction η ps e = (ps', e') : σ, where:
  *   - ps' consists of the patterns of ps annotated with fresh type vars.
  *   - e' is the annotation of e using the types of the patterns in ps'.
  *   - σ is the arrow type for λps'.e'.
  *)
  and annotateFunction 
      (eta : typ Env.env) (ps : Ast.pat list) (e : Ast.exp)
      : pat list * annexp * typ =
  let
    val ps' : pat list = map annotatePat ps
    val eta' = 
      foldr
        (
          fn ((p, sigma), eta') =>
            case p of
                 Ast.PIdent x => Env.update eta' x sigma
               | _ => eta'
        )
        eta
        ps'
    val e' = annotate eta' e
    val resultTy =
      foldr
        (fn ((_, ty), t) => ArrowTy(ty, t))
        (typeOf e')
        ps'
  in
    (ps', e', resultTy)
  end

  and annotateDec (eta : typ Env.env) (d : Ast.dec) : dec * typ Env.env =
  let
    val d' =
      case d of
           Ast.ValDec(p, e) => ValDec(annotatePat p, annotate eta e)
         | Ast.FunDec(f, ps, e) =>
             let
               val fty = Var $ TyVar.fresh()
               val eta' = Env.update eta f fty
               val (ps', e', resultTy) = annotateFunction eta' ps e
             in
               FunDec(f, resultTy, ps', e')
             end
    val eta' =
      case d' of
           ValDec((Ast.PIdent x, sigma), _) => Env.update eta x sigma
         | ValDec(_, _) => eta
         | FunDec(f, fty, _, _) => Env.update eta f fty
  in
    (d', eta')
  end

  
  and annotateDecs (eta : typ Env.env) (ds : Ast.dec list)
      : dec list * typ Env.env =
    case ds of
         [] => ([], eta)
       | d :: ds =>
         let
           val (d', eta') = annotateDec eta d
           val (ds', eta'') = annotateDecs eta' ds
         in
           (d' :: ds', eta'')
         end
  
  fun annotatePgm (eta : typ Env.env) (Ast.Program stms : Ast.pgm) : pgm =
  let
    fun annotateStms (eta : typ Env.env) (stms : Ast.stm list) : stm list =
      case stms of
           [] => []
         | Ast.Dec d :: stms =>
             let
               val (d', eta') = annotateDec eta d
             in
               Dec d' :: annotateStms eta' stms
             end
         | Ast.ExprStm e :: stms =>
             ExprStm (annotate eta e) :: annotateStms eta stms
  in
    Program $ annotateStms eta stms
  end

  (*  ********************
  *   Equations
  *   ********************
  *)

  datatype equation = == of typ*typ
  infix 6 ==

  (*  apply1Eqn (α → σ) (τ == τ') = τ[α → σ] == τ'[α → σ]
  *)
  fun apply1Eqn (b : binding) (tau == tau' : equation) : equation =
    (apply1 b tau) == (apply1 b tau')

  fun applyEqns (b : binding) : equation list -> equation list =
      map $ apply1Eqn b

  fun collectEqns (es : annexp list) : equation list =
    List.concat o map equations $ es

  and equations (AnnExp(e, ty) : annexp) : equation list =
    case (e, ty) of
         (Ident x, _) => []
       | ((Int _ | Real _ | Char _ | Str _ | Bool _), _) => []
       | (Triv, _) => []
       | (Binop(
           (Ast.IPlus | Ast.IMinus | Ast.ITimes | Ast.IDiv | Ast.Mod), e0, e1
         ), _) =>
           typeOf e0 == IntTy :: typeOf e1 == IntTy ::
           collectEqns [e0, e1]
       | (Binop((Ast.RPlus | Ast.RMinus | Ast.RTimes | Ast.RDiv), e0, e1), _) =>
           typeOf e0 == RealTy :: typeOf e1 == RealTy ::
           collectEqns [e0, e1]
       | (Binop((Ast.ILt | Ast.ILe | Ast.IGt | Ast.IGe), e0, e1), _) =>
           typeOf e0 == IntTy :: typeOf e1 == IntTy ::
           collectEqns [e0, e1] 
       | (Binop((Ast.RLt | Ast.RLe | Ast.RGt | Ast.RGe), e0, e1), _) =>
           typeOf e0 == RealTy :: typeOf e1 == RealTy ::
           collectEqns [e0, e1] 
       | (Binop(Ast.Cat, e0, e1), _) =>
           typeOf e0 == StrTy :: typeOf e1 == StrTy ::
           collectEqns [e0, e1]
       | (Binop((Ast.Eq | Ast.Ne), e0, e1), _) =>
             typeOf e0 == typeOf e1 :: collectEqns [e0, e1]
       | (Binop(Ast.Append, e0, e1), _) =>
           typeOf e0 == typeOf e1 :: typeOf e1 == ty ::
           collectEqns [e0, e1]
       | (Binop(Ast.Cons, e0, e1), _) =>
           typeOf e1 == ListTy(typeOf e0) :: typeOf e1 == ty ::
           collectEqns [e0, e1]
       | (Orelse(e0, e1), _) => 
           typeOf e0 == BoolTy :: typeOf e1 == BoolTy ::
           collectEqns [e0, e1]
       | (Andalso(e0, e1), _) => 
           typeOf e0 == BoolTy :: typeOf e1 == BoolTy ::
           collectEqns [e0, e1] 
       | (Cond(e, e0, e1), _) =>
           typeOf e == BoolTy :: typeOf e0 == typeOf e1 :: 
           typeOf e0 == ty ::
           collectEqns [e, e0, e1]
       | (Sel _, _) =>
           raise Fail "Ast.Sel not implemented for type inference."
       | (Tuple es, ProdTy tys) =>
           collectEqns es @
           (map (op==) $ ListPair.zip (map typeOf es, tys))
       | (Tuple es, _) =>
           raise 
             Fail $ "Impossible type for annotate tuple: " ^ typeToString ty
       | (Nil, _) => []
       | (App(e0, e1), _) =>
           typeOf e0 == ArrowTy(typeOf e1, ty) ::
           collectEqns [e0, e1]
       | (Lambda(ps, e), _) => equations e
       | (Let([], e), _) => 
           typeOf e == ty :: equations e 
       | (Let(ValDec((Ast.PIdent x, sigma), e') :: ds, e), _) =>
           sigma == typeOf e' ::
           equations e' @ equations (AnnExp(Let(ds, e), ty))
       | (Let(ValDec((Ast.PWild, sigma), e') :: ds, e), _) =>
           sigma == typeOf e' ::
           equations e' @ equations (AnnExp(Let(ds, e), ty))
       | (Let(FunDec(f, fty, ps, e') :: ds, e), _) =>
           equations e' @ equations (AnnExp(Let(ds, e), ty))

  fun occurs (m : tyvar) (sigma : typ) : bool =
    case sigma of
         Var n => m = n
       | (IntTy | RealTy | StrTy | CharTy | BoolTy | UnitTy) => false
       | ProdTy sigmas => List.exists $ occurs m $ sigmas
       | ListTy sigma => occurs m sigma
       | ArrowTy(sigma, tau) => occurs m sigma orelse occurs m tau

  (* unify eqns = ξ, where ξ is the most general unifier of eqns.
  *)
  fun unify (eqns : equation list) : tysub =
    case eqns of
         [] => []
       | (
           IntTy == IntTy | RealTy == RealTy | StrTy == StrTy 
           | CharTy == CharTy | BoolTy == BoolTy | UnitTy == UnitTy
         ) :: eqns => unify eqns
       | ProdTy sigmas == ProdTy sigmas' :: eqns =>
           unify $ (map (op==) (ListPair.zip(sigmas, sigmas'))) @ eqns
       | ListTy sigma == ListTy sigma' :: eqns =>
           unify (sigma == sigma' :: eqns)
       | ArrowTy(sigma, sigma') == ArrowTy(tau, tau') :: eqns =>
           unify (sigma == tau :: sigma' == tau' :: eqns)
       | (Var m == Var n) :: eqns =>
           if m = n then unify eqns
           else 
             let
               val b = m --> Var n
             in
               b :: (unify $ applyEqns b eqns)
             end
       | (Var m == sigma | sigma == Var m) :: eqns =>
           if occurs m sigma
           then 
             raise
             TypeError (String.concatWith " " [
               "Occurs check fails: ",
               TyVar.toString m,
               "occurs in",
               typeToString sigma
             ])
           else
             let
               val b = m --> sigma
             in
               b :: (unify $ applyEqns b eqns)
             end
       | (sigma == tau) :: _ => raise TypeError (String.concatWith " " [
                "Unification failure:  attempt to unify ",
                typeToString sigma,
                "=",
                typeToString tau
              ])


  (*  checkExp e = e', where e' is the annotated expression corresponding to e.
  *
  *   Raises: 
  *   - TypeError if some declaration or expression statement in p cannot 
  *     be typed for any reason.
  *)
  fun checkExp (e : Ast.exp) : typ =
  let
    val e' = annotate Env.empty e
    val () = 
      dbg_printnl (
        fn () => 
          String.concatWith "\n" o
          (map (fn (x, sigma) => x ^ " : " ^ typeToString sigma)) $
          (getIdAnns e')
      )

    val eqns = equations e'
    val xi = unify eqns
  in
    apply xi (typeOf e')
  end

  fun checkExp' (e : Ast.exp) : annexp =
  let
    val e' = annotate Env.empty e
    val eqns = equations e'
    val xi = unify eqns
  in
    applyAnnExp xi e'
  end

  fun checkPgm' (p as Ast.Program stms : Ast.pgm) : pgm =
  let

    fun getEqns (stm : stm) : equation list =
      case stm of
           Dec(ValDec((_, sigma), e)) =>
             sigma == typeOf e :: equations e 
         | Dec(FunDec(f, fty, ps, e)) =>
             fty ==
               foldr (fn ((_, sigma), r) => ArrowTy(sigma, r)) (typeOf e) ps
             :: equations e
         | ExprStm e =>
             equations e


    val Program stms' = annotatePgm Env.empty p

    val () =
      dbg_printnl (fn () =>
        String.concatWith "\n"
        (map (fn Dec(ValDec((Ast.PIdent x, sigma), _)) => x ^ " : " ^ typeToString sigma
               | Dec(ValDec((Ast.PWild, sigma), _)) => "_ : " ^ typeToString sigma
               | Dec(FunDec(f, fty, _, _)) => f ^ " : " ^ typeToString fty
               | ExprStm e => "<expr> : " ^ typeToString (typeOf e))
             stms')
      )

    val eqns = List.concat $ map getEqns stms'
    val xi = unify eqns

    fun stmsToIdTyp (stms : stm list) : (Ast.ident*typ) list =
      case stms of
           [] => []
         | Dec(ValDec((Ast.PIdent x, sigma), e)) :: stms =>
             (x, apply xi sigma) :: stmsToIdTyp stms
         | Dec(ValDec((Ast.PWild, _), _)) :: stms =>
             stmsToIdTyp stms
         | Dec(FunDec(f, sigma, _, _)) :: stms =>
             (f, apply xi sigma) :: stmsToIdTyp stms
         | ExprStm _ :: stms => stmsToIdTyp stms
  in
    Program $ applyAnnStms xi stms'
  end

  (*  typecheck p = p', where p' is the type-annotated version of p.
  *
  *   Raises: 
  *   - TypeError if some declaration or expression statement in p cannot 
  *     be typed for any reason.
  *)
  fun checkPgm (p as Ast.Program stms : Ast.pgm) : (Ast.ident*typ) list =
  let

    (*
    fun getEqns (stm : stm) : equation list =
      case stm of
           Dec(ValDec((_, sigma), e)) =>
             sigma == typeOf e :: equations e 
         | Dec(FunDec(f, fty, ps, e)) =>
             fty ==
               foldr (fn ((_, sigma), r) => ArrowTy(sigma, r)) (typeOf e) ps
             :: equations e
         | ExprStm e =>
             equations e


    val Program stms' = annotatePgm Env.empty p
    val () =
      dbg_printnl (fn () =>
        String.concatWith "\n"
        (map (fn Dec(ValDec((Ast.PIdent x, sigma), _)) => x ^ " : " ^ typeToString sigma
               | Dec(ValDec((Ast.PWild, sigma), _)) => "_ : " ^ typeToString sigma
               | Dec(FunDec(f, fty, _, _)) => f ^ " : " ^ typeToString fty
               | ExprStm e => "<expr> : " ^ typeToString (typeOf e))
             stms')
      )
    val eqns = List.concat $ map getEqns stms'
    val xi = unify eqns

    fun stmsToIdTyp (stms : stm list) : (Ast.ident*typ) list =
      case stms of
           [] => []
         | Dec(ValDec((Ast.PIdent x, sigma), e)) :: stms =>
             (x, apply xi sigma) :: stmsToIdTyp stms
         | Dec(ValDec((Ast.PWild, _), _)) :: stms =>
             stmsToIdTyp stms
         | Dec(FunDec(f, sigma, _, _)) :: stms =>
             (f, apply xi sigma) :: stmsToIdTyp stms
         | ExprStm _ :: stms => stmsToIdTyp stms
    *)

    (*  This is map o filter, which is why do don't write it as map.
    *)
    fun stmsToIdsTyps (stms : stm list) : (Ast.ident*typ) list =
      case stms of
           [] => []
         | Dec(ValDec((Ast.PIdent x, sigma), e)) :: stms =>
             (x, sigma) :: stmsToIdsTyps stms
         | Dec(ValDec((Ast.PWild, _), _)) :: stms =>
             stmsToIdsTyps stms
         | Dec(FunDec(f, sigma, _, _)) :: stms =>
             (f, sigma) :: stmsToIdsTyps stms
         | ExprStm _ :: stms => stmsToIdsTyps stms


    val Program stms' = checkPgm' p
  in
    stmsToIdsTyps stms'
  end

end
