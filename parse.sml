(*  Parser *)

structure Parse =
struct

  structure T = Tokens

  type stream = MakeLex.stream
  val lex : stream -> T.token*stream = MakeLex.makelex()

  type 'a parse_result = 'a*stream
  type 'a parser = stream -> 'a parse_result

  exception NoParse of string
  exception ExtraTokens of stream

  (* compParser p1 p2 <t_0,...> = ((r1, r2), <t_N,...>), where
  *   p1 <t_0,...> = (r1, <t_n,...>)
  *   p2 <t_n,...> = (r2, <t_N,...>)
  *
  *  In other words, compParser p1 p2 is the parser that first parses with p1,
  *  the parses the remainder with p2, and returns the pair of results.
  *
  *  This is an example of a parser combinator; see the challenge project
  *  description for details.  You do not need to use this for the base
  *  project.
  *)
  fun compParsers (p1 : 'a parser, p2 : 'b parser) : ('a*'b) parser =
    fn strm =>
    let
      val (a, s) = p1 strm
      val (b, s') = p2 s
    in
      ((a, b), s')
    end

  (*  Infix version of compParsers.
  *)
  val <&> = compParsers
  infix <&>

  (* isin t [t_0,...,t_{n-1}] =
   *  if t = t_i for 0 <= i < n, then
   * returns true, else false  *)
  fun isin(x: T.token, xs : T.token list): bool=
      (case xs of
        [] => false
        |y::ys => T.==(x,y) orelse isin(x,ys))

  (*  leftAssoc e [(b_0,e_0),...,(b_{n-1},e_{n-1})] =
   *  b_{n-1}(...b_1(b_0(e, e_0), e_1)..., e_{n-1})  *)
  fun leftAssoc
      (e : Ast.exp)
      (belist  : (Ast.binop*Ast.exp) list) : Ast.exp =
      (case belist of
          [] => e
         |(b,e') :: es' =>
                    leftAssoc (Ast.Binop(b, e, e')) es')

  (*  leftAssoc e [(o_0,e_0),...,(o_{n-1},o_{n-1})] =
   *  o_{n-1}(...o_1(o_0(e, e_0), e_1)..., e_{n-1})  *)
  fun leftAssocB (e : Ast.exp)
      (bolist  : ((Ast.exp*Ast.exp -> Ast.exp)*Ast.exp) list) :
      Ast.exp =
      (case bolist of
           [] => e
         |(bool,e') :: es' =>
                    leftAssocB (bool(e, e')) es')

  fun list_builder
      (s0: stream, par1:stream -> Ast.exp parse_result,
      par2:stream -> (Ast.binop*Ast.exp) list parse_result)
      (x: Ast.binop):(Ast.binop*Ast.exp) list parse_result =
          let
            val (e, s1) = par1 s0
            val (es, s2) = par2 s1
          in
            ((x,e)::es, s2)
          end

  (**********************Expressions_Parser**********************)

  (* parse_exp <t_0,...> = (exp, <t_n,...>), where <t_0,...,t_{n-1}>
   * parses as exp.                                              *)
  fun parse_exp (strm : stream) : Ast.exp parse_result =
      (case lex strm of
        (T.FN, str) => parse_fn(str)
        |(T.IF, str) => parse_if(str)
        |_ =>
        let
            val (e,st) = parse_e strm
            val (ebs,st1) = parse_boollist st
            val (t,st2) = lex st
        in
            (case t of
            T.ORELSE => (leftAssocB e ebs, st1)
            |T.ANDALSO => (leftAssocB e ebs, st1)
            |_ => (e,st))
        end)

  (* parse_boollist <t_0,...> = ([(b_0,e_0),..],<t_n,...>)
   * where <t_0,...,t_{n-1}> returns as a tuple of a
   * list of boolean and exp tuples and the rest of the
   * stream.                           *)
  and parse_boollist(s: stream):
      ((Ast.exp*Ast.exp -> Ast.exp)*Ast.exp) list parse_result =
      (case lex s of
      (T.ORELSE,s0) =>
          (let
            val (e, s1) = parse_e s0
            val (es, s2) = parse_boollist s1
          in
            ((Ast.Orelse, e) :: es, s2)
          end)
      |(T.ANDALSO, s0) =>
          (let
            val (e, s1) = parse_e s0
            val (es, s2) = parse_boollist s1
          in
            ((Ast.Andalso, e) :: es, s2)
          end)
      |_ => ([],s))
      handle Bind => raise NoParse ("Bind - parse_boollist")

  (* parse_fn <t_0,...> = (fnexp,<t_n,...>) where
   * <t_0,...,t_{n-1}> parses to a fn expression of the
   * form fn [p_0,...] => exp where p_i for 0 <= i < n
   * is a pat expression       *)
  and parse_fn(str: stream): Ast.exp parse_result =
      (let
        val (pats, st) = patlist_fn str
        val (T.DARROW, s) = lex st
        val (e, s0) = parse_exp s
      in
        (Ast.Lambda(pats,e),s0)
      end)
      handle Bind => raise NoParse ("Bind - parse_fn")

  (* parse_pat <t_0,t_1...> = (pat,<t_1,...>) where t_0
   * parses as a pat expression.
   * Raises NoParse if t_0 is not a pat expression       *)
  and parse_pat(s: stream): Ast.pat parse_result =
      (case lex s of
      (T.WILD,st) => (Ast.PWild,st)
      |(T.ID(x),st) => (Ast.PIdent(x),st)
      |(t,s) =>
      raise NoParse ("Got:"^T.toString(t)^"-parse_pat"))

  (* patlist_fn <t_0,t_1...> = ([pat_0,...],<t_n,...>) where
   * <t_0,...,t_{n-1}> parses as a list of pat expressions.
  *)
  and patlist_fn(s: stream): Ast.pat list parse_result =
      (let
        val (p0,s0) = parse_pat s
      in
        (case lex s0 of
          (T.DARROW,s1) => ([p0], s0)
          |_ =>
          let
            val (ps, s) = patlist_fn s0
          in
            (p0 :: ps, s)
          end)
      end)

  (* parse_if <t_0,...> = (ifexp,<t_n,...>) where
   * <t_0,..,t_{n-1}> parses to an if expression of the
   * form (if exp then exp else exp)              *)
  and parse_if(str: stream): Ast.exp parse_result =
      (let
          val (e,st) = parse_exp str
          val (T.THEN, s) = lex st
          val (e1,s0) = parse_exp s
          val (T.ELSE, s1) = lex s0
          val (e2,s2) = parse_exp s1
      in
             (Ast.Cond(e,e1,e2),s2)
      end)
      handle Bind => raise NoParse ("Bind - parse_if")

  (* parse_e <t_0,...> = (infexp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as an (infexp op infexp) or an
   * app expression where op is any operator with level 4
   * precedence or higher                             *)
  and parse_e(s : stream) : Ast.exp parse_result =
      (let
        val (e,s0) = parse_c s
        val (ebs,s1) = parse_elist s0
        val (t,s2) = lex s0
        val ls = [T.GT, T.LT, T.GE, T.LE, T.NE, T.EQ,
        T.GTPOUND, T.LTPOUND, T.GEPOUND, T.LEPOUND]
      in
        (case isin(t,ls) of
          true => (leftAssoc e ebs, s1)
          |false => (e,s0))
      end)

  (* parse_elist <t_0,...> = ([(b_0,e_0),...],<t_n,...>), where
   * <t_0,...,t_{n-1}> parses as a sequence of infexp separated
   * by tokens of operations with level 4 precendence
   * (note that the result may be the empty list).          *)
  and parse_elist(s : stream) :
      (Ast.binop*Ast.exp) list parse_result =
      (let
        val (t, s0) = lex s
        val s_par = (s0,parse_c,parse_elist)
      in
        (case t of
        T.GT => list_builder s_par Ast.IGt
        |T.LT => list_builder s_par Ast.ILt
        |T.GE => list_builder s_par Ast.IGe
        |T.LE => list_builder s_par Ast.ILe
        |T.GTPOUND => list_builder s_par Ast.RGt
        |T.LTPOUND => list_builder s_par Ast.RLt
        |T.GEPOUND => list_builder s_par Ast.RGe
        |T.LEPOUND => list_builder s_par Ast.RLe
        |T.NE => list_builder s_par Ast.Ne
        |T.EQ => list_builder s_par Ast.Eq
        |_ => ([], s))
      end)

  (* parse_c <t_0,...> = (infexp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as an (infexp op infexp) or an
   * app expression where op is any operator with level 5
   * precedence or higher                             *)
  and parse_c(s : stream) : Ast.exp parse_result =
      (let
        val (e,s0) = parse_p s
      in
        (case lex s0 of
        (T.DCOLON,s1) =>
          let
            val (es,s2) = parse_c s1
          in
            (Ast.Binop(Ast.Cons,e,es),s2)
          end
        |(T.AT,s1) =>
          let
            val (es,s2) = parse_c s1
          in
            (Ast.Binop(Ast.Append,e,es),s2)
          end
        |_ => (e,s0))
      end)

  (* parse_p <t_0,...> = (infexp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as an (infexp op infexp) or an
   * app expression where op is any operator with level 6
   * precedence or higher                             *)
  and parse_p(s : stream) : Ast.exp parse_result =
      (let
        val (e,s0) = parse_t s
        val (ebs,s1) = parse_plist s0
        val (t,s2) = lex s0
        val ls = [T.PLUS, T.MINUS, T.CAT, T.PLUSPOUND, T.MINUSPOUND]
      in
        case isin(t,ls) of
        true => (leftAssoc e ebs, s1)
        |false => (e,s0)
      end)

  (* parse_plist <t_0,...> = ([(b_0,e_0),...],<t_n,...>), where
   * <t_0,...,t_{n-1}> parses as a sequence of infexp separated
   * by tokens of operations with level 6 precendence
   * (note that the result may be the empty list).          *)
  and parse_plist(s : stream) :
      (Ast.binop*Ast.exp) list parse_result =
      (let
        val (t, s0) = lex s
        val s_par = (s0,parse_t,parse_plist)
      in
        case t of
        T.PLUS => list_builder s_par Ast.IPlus
        |T.MINUS => list_builder s_par Ast.IMinus
        |T.PLUSPOUND => list_builder s_par Ast.RPlus
        |T.MINUSPOUND => list_builder s_par Ast.RMinus
        |T.CAT => list_builder s_par Ast.Cat
        |_ => ([],s)
      end)

  (* parse_t <t_0,...> = (infexp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as an (infexp op infexp) or an
   * app expression where op is any operator with level 7
   * precedence or higher                             *)
  and parse_t(s : stream) : Ast.exp parse_result =
      (let
        val (e,s0) = parse_app s
        val (ebs,s1) = parse_tlist s0
        val (t,s2) = lex s0
        val ls = [T.TIMES, T.DIV, T.MOD, T.DIVPOUND, T.TIMESPOUND]
      in
        case isin(t,ls) of
         true => (leftAssoc e ebs, s1)
        |false => (e,s0)
      end)

  (* parse_tlist <t_0,...> = ([(b_0,e_0),...],<t_n,...>), where
   * <t_0,...,t_{n-1}> parses as a sequence of infexp separated
   * by tokens of operations with level 7 precendence
   * (note that the result may be the empty list).          *)
  and parse_tlist(s : stream) :
      (Ast.binop*Ast.exp) list parse_result =
        (let
          val (t, s0) = lex s
          val s_par = (s0,parse_app,parse_tlist)
        in
          case t of
          T.TIMES => list_builder s_par Ast.ITimes
          |T.DIV => list_builder s_par Ast.IDiv
          |T.MOD => list_builder s_par Ast.Mod
          |T.DIVPOUND => list_builder s_par Ast.RDiv
          |T.TIMESPOUND => list_builder s_par Ast.RTimes
          |_ => ([],s)
        end)

  (* parse_app <t_0,...> = (appexp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as an atexp or an
   * appexp atexp which is a sequence of atexp    *)
  and parse_app(s : stream) : Ast.exp * stream =
      (let
        val (at,st) = parse_at s
      in
        (case lex st of
          (T.LBRACK, s0) => app_help st at
          | (T.LP, s0) => app_help st at
          | (T.LET,s0) => app_help st at
          | (T.CHAR c, s0) => app_help st at
          | (T.INT n, s0) => app_help st at
          | (T.REAL n, s0) => app_help st at
          | (T.TRUE, s0) => app_help st at
          | (T.FALSE, s0) => app_help st at
          | (T.STRING w, s0) => app_help st at
          | (T.SEL n, s0) => app_help st at
          | (T.NIL, s0) => app_help st at
          | (T.ID x, s0) => app_help st at
          |_ => (at,st))
      end)

  and app_help(st: stream)(at: Ast.exp):
      Ast.exp * stream =
      (let
          val (apps, s) = parse_app st
        in
          (Ast.App(at,apps), s)
        end)

  (* parse_at <t_0,...> = (atexp, <t_n,...>) where <t_0,...,t_{n-1}>
   * parses as an atexp.
   * Raises NoParse if there is no n such that <t_0,...,t_{n-1}>
   * parses as an atexp.*)
  and parse_at(s : stream) : Ast.exp * stream =
      (case lex s of
          (T.LBRACK, s0) => parse_brack s0
          | (T.LP, s0) => parse_paren s0
          | (T.LET,s0) => parse_let s0
          | (T.SEL n, s0) => (Ast.Sel n, s0)
          | (T.CHAR c, s0) => (Ast.Char c, s0)
          | (T.INT n, s0) => (Ast.Int n, s0)
          | (T.REAL n, s0) => (Ast.Real n, s0)
          | (T.TRUE, s0) => (Ast.Bool(true), s0)
          | (T.FALSE, s0) => (Ast.Bool(false), s0)
          | (T.STRING w, s0) => (Ast.Str w, s0)
          | (T.NIL, s0) => (Ast.Nil, s0)
          | (T.ID x, s0) => (Ast.Ident x, s0)
          | (t,s) =>
            raise NoParse("Got " ^ T.toString(t) ^ " at parse_at"))

  (* parse_paren <t_0,...> = (exp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as either a sequence of expressions
   * in () or a single expression *)
  and parse_paren(s : stream) : Ast.exp * stream =
      (case lex s of
        (T.RP,s0) => (Ast.Triv,s0)
        |_ =>
            let
              val (e,s1) = parse_exp s
            in
              (case lex s1 of
                (T.RP,s2) => (e,s2)
                |(T.COMMA,s2) =>
                let
                  val (es,s3) = exp_paren s1
                  val (T.RP,s4) = lex s3
                in
                  (Ast.Tuple(e::es),s4)
                end
                |(t,s) =>
                raise NoParse ("Got:"^T.toString(t)^"-parse_paren"))
            end)

  (* exp_paren <t_0,...> = ([e_0,...],<t_n,...>) where the
   * <t_0,...,t_{n-1}> parses as a sequence of exp separated by a
   * T.COMMA (note that the result may be the empty list)   *)
  and exp_paren(s: stream): Ast.exp list parse_result =
      (case lex s of
        (T.COMMA, s0) =>
            let val (e1,s1) = parse_exp s0
                val (es,s2) = exp_paren s1
            in
                (e1::es,s2)
            end
        |_ => ([],  s))

  (* parse_brack <t_0,...> = (exp, <t_n,...>) where
   * <t_0,...,t_{n-1}> parses as either a sequence of expressions
   * in [] or a single expression *)
  and parse_brack(s: stream) : Ast.exp parse_result =
      (case lex s of
        (T.RBRACK, s0) => (Ast.Nil,s0)
        |_ =>
          let
            val (e,s1) = parse_exp s
          in
            (case lex s1 of
              (T.RBRACK,s2) =>
              (Ast.Binop(Ast.Cons,e,Ast.Nil),s2)
              |(T.COMMA,s2) =>
              let
                val (e1,s3) = parse_brack s2
              in
                (Ast.Binop(Ast.Cons,e,e1),s3)
              end
              |(t,s) =>
              raise NoParse ("Got:"^T.toString(t)^"-parse_brack"))
          end)

  (* parse_let <t_0,...> = (atexp, <t_n,...>) where <t_0,...,t_{n-1}>
   * parses an atexp of the form let [d_0,...] in exp end where
   * d_i is a declaration *)
  and parse_let(s: stream): Ast.exp parse_result =
      (let
        val (ds, s0) = dec_seq s
        val (exp, s2) = parse_exp s0
        val (T.END,s3) = lex s2
      in
        (Ast.Let(ds,exp),s3)
      end)
      handle Bind => raise NoParse ("Bind - parse_let")

  (* dec_seq <t_0,...> = ([d_0,...],<t_n,...>) where the
   * <t_0,...,t_{n-1}> parses as a sequence of dec that
   * could be separated by a T.SEMI
   * (note that the result may be the empty list)   *)
  and dec_seq(s: stream) : Ast.dec list parse_result =
      (let
          val (d,s0) = parse_dec s
        in
          (case lex s0 of
            (T.IN, s1) => (d,s1)
            |(T.SEMI,s1)=>
            let
              val (ds,s2) = dec_seq s1
            in
              (d@ds,s2)
            end
            |_ =>
            let
              val (ds,s1) = dec_seq s0
            in
              (d@ds,s1)
            end)
        end)

  (* parse_dec <t_0,...> = ([dec], <t_n,...>) where <t_0,...,t_{n-1}>
   * parses a single element dec list where the dec is of the
   * form val pat = exp or fun x [p_0,...] = exp where p_i in 0<=i<n
   * is a pat expression                            *)
  and parse_dec(s: stream) : Ast.dec list parse_result =
      (case lex s of
        (T.VAL, s0) =>
          (let
            val (pat, s1) = parse_pat s0
            val (T.EQ, s2) = lex s1
            val (exp, s3) = parse_exp s2
          in
            ([Ast.ValDec(pat,exp)], s3)
          end)
        |(T.FUN, s0) =>
          (let
            val (T.ID(x),s1) = lex s0
            val (ps, s2) = patlist_fun s1
            val (T.EQ, s3) = lex s2
            val (exp, s4) = parse_exp s3
          in
            ([Ast.FunDec(x,ps,exp)], s4)
          end)
        |_ => ([],s))
        handle Bind => raise NoParse ("Bind - parse_dec")

  (* patlist_fun <t_0,...> = ([p_0,...],<t_n,...>) where the
   * <t_0,...,t_{n-1}> parses as a sequence of pat expressions *)
  and patlist_fun(s: stream): Ast.pat list parse_result =
      (let
        val (p0,s0) = parse_pat s
      in
        (case lex s0 of
          (T.EQ,s1) => ([p0], s0)
          |_ =>
          let
            val (ps, s) = patlist_fun s0
          in
            (p0 :: ps, s)
          end)
      end)


  (******************Program_Parser**************************)
  (* parse_pgm <t_0,...> = (p, <t_n,...>), where <t_0,...,t_{n-1}>
   * parses as a program.
   * Raises NoParse if there is no n such that <t_0,...t_{n-1}>
   * parses as a program.                                      *)
  fun parse_pgm (strm : stream) : Ast.pgm parse_result =
    (let
      val (ment, st) =  parse_state strm
      val (T.SEMI,s) = lex st
    in
      (case lex s of
          (T.EOF, s0) => (Ast.Program([ment]),s)
          |_ =>
            let
              val (ap, s1) = parse_pgm s
            in
              case ap of
              Ast.Program(sl) => (Ast.Program(ment :: sl), s1)
            end)
    end)
    handle Bind => raise NoParse ("Bind - parse_pgm")

  (* parse_state <t_0,...> = (st, <t_n,...>), where <t_0,...,t_{n-1}>
   * parses as a statement.
   * Raises NoParse if there is no n such that <t_0,...t_{n-1}>
   * parses as a statement.                                       *)
  and parse_state (strm: stream): Ast.stm parse_result =
    (case lex strm of
      (T.VAL,s) =>
        let
          val (pat, s1) = parse_pat s
          val (T.EQ, s2) = lex s1
          val (exp, s3) = parse_exp s2
        in
          (Ast.Dec(Ast.ValDec(pat,exp)),s3)
        end
      |(T.FUN,s) =>
        let
          val (T.ID(x),s1) = lex s
          val (ps, s2) = patlist_fun s1
          val (T.EQ, s3) = lex s2
          val (exp, s4) = parse_exp s3
        in
          (Ast.Dec(Ast.FunDec(x,ps,exp)),s4)
        end
      |_ =>
        let
          val (e,s0) = parse_exp strm
        in
          (Ast.ExprStm(e),s0)
        end)
      handle Bind => raise NoParse ("Bind - parse_state")

  (* ****************************************
  *  Client-visible parsing functions.
  *  ****************************************
  *)

  fun parseExp (strm : stream) : Ast.exp =
  let
    val (e, s) = parse_exp strm
  in
    case lex s of
         (T.EOF, _) => e
       | _ => raise ExtraTokens s
  end

  fun parsePgm (strm : stream) : Ast.pgm =
  let
    val (p, s) = parse_pgm strm
  in
    case lex s of
         (T.EOF, _) => p
       | _ => raise ExtraTokens s
  end

end