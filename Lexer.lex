{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "begin"	=> Parser.BEGIN pos
       | "end"          => Parser.END pos
       | "call"		=> Parser.CALL pos
       | "uncall"	=> Parser.UNCALL pos
       | "reveal"	=> Parser.REVEAL pos
       | "hide"		=> Parser.HIDE pos
       | "p8"		=> Parser.P8 pos
       | "p16"		=> Parser.P16 pos
       | "p32"		=> Parser.P32 pos
       | "p64"		=> Parser.P64 pos
       | "s8"		=> Parser.S8 pos
       | "s16"		=> Parser.S16 pos
       | "s32"		=> Parser.S32 pos
       | "s64"		=> Parser.S64 pos
       | _              => Parser.NAME (s, pos);

 }

rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
  | "//" [^`\n`]*	{ Token lexbuf } (* comment *)
  | "/*" ([^`*`] | `*`[^`/`])* "*/"
		 	{ Token lexbuf } (* comment *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | `~`?[`0`-`9`]+      { Parser.INT (getLexeme lexbuf, getPos lexbuf) }  
  | [`a`-`z``A`-`Z`][`a`-`z``A`-`Z``0`-`9`]*
                        { keyword (getLexeme lexbuf, getPos lexbuf) }
  | "->"                { Parser.RARR (getPos lexbuf) }
  | "<-"                { Parser.LARR (getPos lexbuf) }
  | "<->"		{ Parser.SWAP (getPos lexbuf) }
  | ":="		{ Parser.ASSIGN (getPos lexbuf) }
  | "=="                { Parser.EQ (getPos lexbuf) }
  | "!="                { Parser.NE (getPos lexbuf) }
  | `>`                 { Parser.GT (getPos lexbuf) }
  | `<`                 { Parser.LT (getPos lexbuf) }
  | ">="                { Parser.GE (getPos lexbuf) }
  | "<="                { Parser.LE (getPos lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | `*`                 { Parser.TIMES (getPos lexbuf) }
  | `/`                 { Parser.DIV (getPos lexbuf) }
  | `%`                 { Parser.MOD (getPos lexbuf) }
  | `^`			{ Parser.XOR (getPos lexbuf) }
  | "<<"		{ Parser.LSHIFT (getPos lexbuf) }
  | ">>"		{ Parser.RSHIFT (getPos lexbuf) }
  | "+="		{ Parser.PLUSEQ (getPos lexbuf) }
  | "-="		{ Parser.MINUSEQ (getPos lexbuf) }
  | "^="		{ Parser.XOREQ (getPos lexbuf) }
  | "<<="		{ Parser.LSHIFTEQ (getPos lexbuf) }
  | ">>="		{ Parser.RSHIFTEQ (getPos lexbuf) }
  | `|`                 { Parser.BAR (getPos lexbuf) }
  | `&`                 { Parser.AMPERSAND (getPos lexbuf) }
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `[`                 { Parser.LBRACKET (getPos lexbuf) }
  | `]`                 { Parser.RBRACKET (getPos lexbuf) }
  | `,`                 { Parser.COMM (getPos lexbuf) }
  | `:`			{ Parser.COLON (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }
;
