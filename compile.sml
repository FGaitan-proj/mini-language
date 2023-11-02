(*  Compiler *)

structure Compile =
struct

  structure TIO = TextIO

  structure Type = SType

  (* Debugger *)
  val dbg_printnl = Util.dbg_printnl
  val $ = Util.$
  infix 0 $

  (* Compiler environment *)
      (* rho(x) = the store location for x.
      * nextLoc = a store location such that all locations ≥ nextLoc are
      * free.
      *)
  structure CompEnv = struct
    type env = {
      rho : int Env.env,
      nextLoc : int
    }

    (* empty environment *)
    val empty : env =
      {
        rho=Env.empty,
        nextLoc=0
      }

    fun get ({rho,...} : env) (x : Ast.ident) : int =
      Env.get rho x

    fun insert ({rho,nextLoc} : env) (x : Ast.ident) : env*int =
    (
      {
        rho=Env.update rho x nextLoc,
        nextLoc=nextLoc+1
      },
      nextLoc
    )

  end

  structure var = Type.TyVar

  type compile_env = CompEnv.env


  (*  **********
  *   String construction and output functions.
  *   **********
  *
  *   You might find these useful.
  *)

  (*  emit s emits s ^ "\n" to outs.
  *)
  fun emit (outs : TIO.outstream) (s : string) : unit =
    TextIO.output(outs, s ^ "\n") before TextIO.flushOut(outs)

  (*  emitNoNL s emits s to outs.
  *)
  fun emitNoNL (outs : TIO.outstream) (s : string) : unit =
    TextIO.output(outs, s) before TextIO.flushOut(outs)

  (*  makeParts [s_0,...,s_{n-1}] = s_0 ^ " " ^ ... ^ " " s_{n-1}.
  *)
  val makeParts : string list -> string = String.concatWith " "

  (*  emitParts = emit o makeParts
  *)
  fun emitParts (outs : TIO.outstream) : string list -> unit =
    emit outs o makeParts

  (*  emitLines [l_0,...l_{n-1}] =
  *     (emit l_0 ; emit l_1 ; ... ; emit l_{n-1})
  *)
  fun emitLines (outs : TIO.outstream) : string list -> unit =
    emit outs o (String.concatWith "\n")

  (*  makeLbl l = l ^ ":"
  *)
  fun makeLbl (l : string) =
    l ^ ":"

  (*  makeComment c = ";" ^ c
  *)
  fun makeComment (c : string) =
    "; " ^ c

  (*  makeLine [s_0,...,s_{n-1}] = s_0 ^ " " ^ ... ^ " " ^ s_{n-1}.
  *)
  val makeLine : string list -> string =
    String.concatWith " "

  (* ItoStr x = "Ax"
  *)
  fun ItoStr (i: int): string =
      "A"^Int.toString(i)

  (*LtoList s = ["s:"]*)
  fun LtoList (s : string): string list =
      [s^":"]

  (*TauRep(t) = (s,s') where s is the input type bytecode representation
  and s' is the output type bytecode representation*)
  fun TauRep(tau: Type.typ): string*string =
    case tau of
      Type.IntTy => ("","I")
      |Type.RealTy => ("","D")
      |Type.BoolTy => ("","Z")
      |Type.UnitTy => ("","V")
      |Type.ArrowTy(k,t1) =>
          let
            val (inp,outp) = TauRep(t1)
          in
            case k of
            Type.IntTy => ("I"^inp,outp)
            |Type.RealTy => ("D"^inp,outp)
            |Type.BoolTy => ("Z"^inp,outp)
            |Type.UnitTy => ("V"^inp,outp)
          end
      |_ => ("","V")

  (*  compileExp' eta e' = lines, where lines is a sequence of Jasmin
  *  instructions for computing e' based on compilation environment eta.
  *)
  fun compileExp'
      (eta : compile_env)
      (e' as Type.AnnExp(e, tau) : Type.annexp) : string list =
      let
        val (v_1, v_2) = (ItoStr(var.fresh()), ItoStr(var.fresh()))
        val end_c =  LtoList(v_2) @ ["nop"]
      in
        case e of
             Type.Ident x =>
                (case tau of
                Type.IntTy => ["iload " ^ Int.toString(CompEnv.get eta x)]
                |Type.RealTy => ["dload " ^ Int.toString(CompEnv.get eta x)]
                |Type.BoolTy => ["iload " ^ Int.toString(CompEnv.get eta x)]
                |_ => raise Fail x)
           | Type.Triv => [""]
           | Type.Int n =>
                (case (n >= 0) of
                  true => ["ldc " ^ Int.toString n]
                  |false => ["ldc -" ^ (Int.toString (~1*n))])
           | Type.Real n => (*turn double into float with d2f*)
               (case (Real.>=(n,0.0)) of
                 true => ["ldc2_w " ^ Real.toString n]
                 |false => ["ldc2_w -" ^ (Real.toString (~1.0 * n))])
           | Type.Bool n =>
              (case n of
                true => ["ldc 1"]
                |false => ["ldc 0"])
           | Type.Binop(b,e0,e1) =>
              compileExp' eta e0 @
              compileExp' eta e1 @
              (compileBinop eta (b,e0,e1,tau))
           | Type.Orelse(e0,e1) =>
               let
                 val if_false = ["ifeq "^v_1 , "ldc 1", "goto "^v_2]
                 val e1_c = LtoList(v_1) @ (compileExp' eta e1)
               in
                 (compileExp' eta e0) @ if_false @ e1_c @ end_c
               end
           | Type.Andalso(e0,e1) =>
               let
                 val if_false = ["ifeq " ^ v_1] @ (compileExp' eta e1)
                 val e1_c = ["goto " ^ v_2] @ LtoList(v_1) @ ["ldc 0"]
               in
                 (compileExp' eta e0) @ if_false @ e1_c @ end_c
               end
           | Type.Cond(e0,e1,e2) =>
              let
                val if_else = ["ifeq "^ v_1] @ (compileExp' eta e1)
                val e2_c = ["goto "^ v_2] @ LtoList(v_1) @ (compileExp' eta e2)
              in
                (compileExp' eta e0) @ if_else @ e2_c @ end_c
              end
           | Type.Let(pl, e1) => (compileVal eta pl e1)
           | Type.App(e0,e1) =>
                (compileApps eta e0 e1 v_1 v_2 end_c [])


    end

  and compileApps
      (eta : compile_env)
      (e0: Type.annexp) (e1: Type.annexp)
      (v_1: string) (v_2: string)
      (end_c: string list)
      (compile1: string list): string list =
        case e0 of
        Type.AnnExp(Type.Ident(k),ty) =>
          (case k of
            "printInt" => (compileExp' eta e1) @ compile1 @
              ["invokestatic CSupport/printInt(I)V"]
            |"printReal" => (compileExp' eta e1) @ compile1 @
              ["invokestatic CSupport/printDouble(D)V"]
            |"printBool" => (compileExp' eta e1) @ compile1 @
              ["invokestatic CSupport/printBool(Z)V"]
            |"not" => (compileExp' eta e1) @ compile1 @
              ["ifeq "^v_1, "ldc 0","goto "^v_2] @
              LtoList(v_1) @ ["ldc 1"] @ end_c
            |"printString" => raise Fail "str"
            |_ => (compileExp' eta e1) @ compile1 @
            let
              val (inp,outp) = TauRep ty
            in
            ["invokestatic C/"^k^"("^inp^")"^outp]
            end)
        |Type.AnnExp(Type.App(e2,e3),ty) =>
            let
              val comp1 = (compileExp' eta e1) @ compile1
            in
            (compileApps eta e2 e3 v_1 v_2 end_c comp1)
            end

  and compileVal
      (eta : compile_env)
      (pl: Type.dec list)
      (e1: Type.annexp) : string list =
      case pl of
        [Type.ValDec((Ast.PIdent(x),ty),e0)] =>
          let
            val e0_c = compileExp' eta e0
            val (eta',curr) = CompEnv.insert eta x
          in
            e0_c @ ["istore "^(Int.toString curr)] @ (compileExp' eta' e1)
          end
        |[Type.ValDec((Ast.PWild,ty),e0)] =>
            (compileExp' eta e0) @ (compileExp' eta e1)
        |(Type.ValDec((Ast.PIdent(x),ty),e0)::pls) =>
          let
            val e0_c = compileExp' eta e0
            val (eta',curr) = CompEnv.insert eta x
          in
            e0_c @ ["istore "^(Int.toString curr)] @ (compileVal eta' pls e1)
          end
        |(Type.ValDec((Ast.PWild,ty),e0)::pls) =>
              (compileExp' eta e0) @ (compileVal eta pls e1)


  and compileBinop(eta: compile_env)
      (b: Ast.binop, Type.AnnExp(e0,t0): Type.annexp,
      Type.AnnExp(e1,t1): Type.annexp, tau: Type.typ): string list =
      let
        val (tru, don) = (ItoStr(var.fresh()), ItoStr(var.fresh()))
        val rest = ["ldc 0","goto "^ don,
                    makeLbl(tru),"ldc 1", makeLbl(don), "nop"]
      in
        case b of
         Ast.IPlus => ["iadd"]
        | Ast.IMinus => ["isub"]
        | Ast.ITimes => ["imul"]
        | Ast.IDiv => ["idiv"]
        | Ast.RPlus => ["dadd"]
        | Ast.RMinus => ["dsub"]
        | Ast.RTimes => ["dmul"]
        | Ast.RDiv => ["ddiv"]
        | Ast.Mod => ["irem"]
        | Ast.ILt => ["if_icmplt " ^ tru] @ rest
        | Ast.ILe => ["if_icmple " ^ tru] @ rest
        | Ast.IGt => ["if_icmpgt " ^ tru] @ rest
        | Ast.IGe => ["if_icmpge " ^ tru] @ rest
        | Ast.RLt => ["dcmpg","iflt " ^ tru] @ rest
        | Ast.RLe =>  ["dcmpg","ifle " ^ tru] @ rest
        | Ast.RGt =>  ["dcmpg","ifgt " ^ tru] @ rest
        | Ast.RGe =>  ["dcmpg","ifge " ^ tru] @ rest
        | Ast.Eq =>
            (case t0 of
              Type.IntTy => ["if_icmpeq " ^ tru] @ rest
              |Type.RealTy => ["dcmpg","ifeq " ^ tru] @ rest
              |Type.BoolTy => ["if_icmpeq " ^ tru] @ rest)
        | Ast.Ne =>
            (case t0 of
              Type.IntTy => ["if_icmpne " ^ tru] @ rest
              |Type.RealTy => ["dcmpg","ifne " ^ tru] @ rest
              |Type.BoolTy => ["if_icmpne " ^ tru] @ rest)
      end

  
  
  (* Program-visible functions *)

  (*  compileExp outs e emits a Jasmin program to outs that computes the
  *  value of e and prints the result to the terminal.
  *)
  fun compileExp (outs : TIO.outstream) (e : Type.annexp) : unit =
  let
    val emit = emit outs
    val emitNoNL = emitNoNL outs
    val emitlines : string list -> unit = List.app emit

    (*  emitLbl L = emit (lbl ^ "\n")
    *)
    fun emitLbl (lbl : string) : unit =
      emit (lbl ^ ":")


    (*  emitClassHeader() emits the standard class header to outs.
    *)
    fun emitClassHeader() : unit =
      emitlines
      [
        ".class public C",
        ".super java/lang/Object",
        ""
      ]

    fun emitMain(body : string list, ty : Type.typ) : unit =
    (
      emitlines
      [
        ".method public static main([Ljava/lang/String;)V",
        ".limit stack 1000",
        ".limit locals 1000"
      ] ;
      emitlines body ;
      emitlines
      [
        case ty of
             Type.IntTy => "invokestatic CSupport/printInt(I)V"
           | Type.RealTy => "invokestatic CSupport/printDouble(D)V"
           | Type.BoolTy => "invokestatic CSupport/printBool(Z)V"
           | Type.UnitTy => ""
           | _ => raise Fail "Bad final type.",
        "return",
        ".end method"
      ]
    )
  in
    let
      val () = emitClassHeader()
      val () =
        emitMain (compileExp' CompEnv.empty e, Type.typeOf e)
    in
      ()
    end
  end


  (*  compilePgm outs p emits a Jasmin program to outs that is equivalent to
  *  the program p. calls compileExp' on rhs of functions
  *)
  fun compilePgm (outs : TIO.outstream) (Type.Program ds : Type.pgm) : unit =
  let
    val emit = emit outs
    val emitNoNL = emitNoNL outs
    val emitlines : string list -> unit = List.app emit

    (* InputTypes([p_1,..]) = "sss.." where s in position i corresponds
      to the type of p_i
    *)
    fun InputTypes (pl : Type.pat list) (env : compile_env):
        string * compile_env =
        case pl of
        [(p,ty)] =>
        (case p of
          Ast.PIdent(i) =>
            let
              val (env',num) = CompEnv.insert env i
            in
              case ty of
                Type.UnitTy => ("V",env')
                |Type.IntTy => ("I",env')
                |Type.RealTy => ("D",env')
                |Type.BoolTy => ("Z",env')
            end
          |Ast.PWild => ("V", env)
          )
        |(p,ty)::pls =>
        (case p of
          Ast.PIdent(i) =>
            let
              val (env',num) = CompEnv.insert env i
              val (str,rho) = InputTypes pls env'
            in
              case ty of
                Type.UnitTy => ("V"^str,rho)
                |Type.IntTy => ("I"^str,rho)
                |Type.RealTy => ("D"^str,rho)
                |Type.BoolTy => ("Z"^str,rho)
            end
          |Ast.PWild => ("V", env)
          )


    (* OutputTypes(ty) = "s" where s is the bytecode type corresponding to
    ty
    *)
    fun OutputTypes(ty: Type.typ): string =
        case ty of
        Type.IntTy => "I"
        | Type.RealTy  => "D"
        | Type.BoolTy => "Z"
        | Type.UnitTy => "V"
        |_ => raise Fail "OutputTypes"

    (*  emitClassHeader() emits the standard class header to outs.
    *)
    fun emitClassHeader() : unit =
      emitlines
      [
        ".class public C",
        ".super java/lang/Object",
        ""
      ]

    fun emitMain(body : string list,ty : Type.typ): unit =
    (
      emitlines
      [
        ".method public static main([Ljava/lang/String;)V",
        ".limit stack 1000",
        ".limit locals 1000"
      ] ;
      emitlines body ;
      emitlines
      [
        case ty of
             Type.IntTy => "invokestatic CSupport/printInt(I)V"
           | Type.RealTy => "invokestatic CSupport/printDouble(D)V"
           | Type.BoolTy => "invokestatic CSupport/printBool(Z)V"
           | Type.UnitTy => ""
           | _ => raise Fail ("Bad Main Type:"^Type.typeToString(ty)),
        "return",
        ".end method"
      ]
    )

    fun emitMethod
    (name: string) (inp: string)
    (body : string list) (ty : Type.typ) : unit =
      (
        emitlines
        [
          ".method public static "^name^"("^inp^")"^OutputTypes(ty)^"",
          ".limit stack 100",
          ".limit locals 100"
        ] ;
        emitlines body ;
        emitlines
        [
          case ty of
              Type.RealTy => "dreturn"
             | Type.UnitTy => "return"
             | _ => "ireturn",
          ".end method"
        ]
      )



   fun compilePgm'(stms : Type.stm list): unit =
        case stms of
        [Type.Dec(Type.ValDec(p,e))] => ()
        |[Type.ExprStm(e)] => ()
        |Type.Dec(Type.ValDec(p,e)) :: ds => compilePgm' ds
        |Type.ExprStm(e) :: ds => compilePgm' ds
        |[Type.Dec(Type.FunDec(x,t,pl,e))] =>
          let
            val (input, rho) = InputTypes pl (CompEnv.empty)
            val body = compileExp' rho e
            val ty = Type.typeOf e
          in
            case x of
            "main" => emitMain(body,ty)
            |_ => emitMethod x input body ty
          end
        |Type.Dec(Type.FunDec(x,t,pl,e)) :: ds =>
          let
            val (input, rho) = InputTypes pl (CompEnv.empty)
            val body = compileExp' rho e
            val ty = Type.typeOf e
          in
            (case x of
            "main" =>
                let
                  val () = emitMain(body,ty)
                  val () = compilePgm' ds
                in () end
            |_ =>
                let
                  val () = emitMethod x input body ty
                  val () = compilePgm' ds
                in () end
            )
          end
    in
      let
        val () = emitClassHeader()
        val () = compilePgm' ds
      in
        ()
      end
    end

end
