(* Interpreter *)
(* compile with mosmlc hi.sml -o hi *)
structure Main =
struct

  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer ( fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun errorMess s = TextIO.output (TextIO.stdErr,s ^ "\n");

  fun hs filename backwards =  
      let
        val lexbuf = createLexerStream
			  (BasicIO.open_in (filename ^ ".cl"))
      in
        let
          val pgm = Parser.Program Lexer.Token lexbuf
          (*val _ = TextIO.output (TextIO.stdErr, Data.showProgram pgm)*)
        in
          TextIO.output (TextIO.stdErr,Data.showProgram pgm) ;
          Types.check pgm ;
          Interpreter.run pgm backwards
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
               | Types.Error (mess, (lin,col)) =>
                     errorMess ("Type error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
               | Interpreter.Error (mess, (lin,col)) =>
                     errorMess ("Runtime error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
               | BigInt.BigIntError (mess, (lin,col)) =>
                     errorMess ("Numeric error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
               | SysErr (s,_) => errorMess ("Exception: " ^ s)
               | Subscript =>  errorMess "subscript error"
      end

  val _ =
    let val cmdLine = Mosml.argv () in
      if List.length cmdLine = 2
      then hs (List.nth(cmdLine,1)) false
      else if List.length cmdLine = 3 andalso List.nth(cmdLine,1) = "-r"
      then hs (List.nth(cmdLine,2)) true
      else if List.length cmdLine = 3 andalso List.nth(cmdLine,2) = "-r"
      then hs (List.nth(cmdLine,1)) true
      else  errorMess "call by, e.g., hi TEA or hi -r TEA"
    end
end
