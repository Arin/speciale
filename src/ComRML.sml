structure ComRML =
struct

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n");

  fun rs filename =  
      let
        val lexbuf = createLexerStream
			  (BasicIO.open_in (filename ^ ".rml"))
      in
        let
          val pgm = Parser.Prog Lexer.Token lexbuf
          val outstream = TextIO.openOut (filename ^ ".c")
	   (* val () = Type.checkProgram pgm *)
        in
          TextIO.output(outstream, Compiler.doProg pgm)
        end
          handle Parsing.yyexit ob => errorMess "Parser-exit\n"
               | Parsing.ParseError ob =>
                   let val (lin,col) = Lexer.getPos lexbuf
                   in
                     errorMess ("Parse-error at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
                   end
               | Lexer.LexicalError (mess,(lin,col)) =>
                     errorMess ("Lexical error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
               | SysErr (s,_) => errorMess ("Exception: " ^ s)
      end

  val _ = (rs (List.nth(Mosml.argv (),1)))
          handle Subscript => errorMess "call by, e.g.  comRML hash"

end
