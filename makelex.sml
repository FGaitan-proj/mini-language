structure MakeLex =
struct

  type stream = Lexer.strm

  fun makelex() =
  let
    val sm = AntlrStreamPos.mkSourcemap()
  in
    fn strm => 
      case Lexer.lex sm strm of
           (t, _, s) => (t, s)
  end

end
