local open Obj Lexing in


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

 
fun action_38 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_37 lexbuf = (
 Parser.EOF (getPos lexbuf) )
and action_36 lexbuf = (
 Parser.COLON (getPos lexbuf) )
and action_35 lexbuf = (
 Parser.COMM (getPos lexbuf) )
and action_34 lexbuf = (
 Parser.RBRACKET (getPos lexbuf) )
and action_33 lexbuf = (
 Parser.LBRACKET (getPos lexbuf) )
and action_32 lexbuf = (
 Parser.RPAR (getPos lexbuf) )
and action_31 lexbuf = (
 Parser.LPAR (getPos lexbuf) )
and action_30 lexbuf = (
 Parser.AMPERSAND (getPos lexbuf) )
and action_29 lexbuf = (
 Parser.BAR (getPos lexbuf) )
and action_28 lexbuf = (
 Parser.RSHIFTEQ (getPos lexbuf) )
and action_27 lexbuf = (
 Parser.LSHIFTEQ (getPos lexbuf) )
and action_26 lexbuf = (
 Parser.XOREQ (getPos lexbuf) )
and action_25 lexbuf = (
 Parser.MINUSEQ (getPos lexbuf) )
and action_24 lexbuf = (
 Parser.PLUSEQ (getPos lexbuf) )
and action_23 lexbuf = (
 Parser.RSHIFT (getPos lexbuf) )
and action_22 lexbuf = (
 Parser.LSHIFT (getPos lexbuf) )
and action_21 lexbuf = (
 Parser.XOR (getPos lexbuf) )
and action_20 lexbuf = (
 Parser.MOD (getPos lexbuf) )
and action_19 lexbuf = (
 Parser.DIV (getPos lexbuf) )
and action_18 lexbuf = (
 Parser.TIMES (getPos lexbuf) )
and action_17 lexbuf = (
 Parser.MINUS (getPos lexbuf) )
and action_16 lexbuf = (
 Parser.PLUS (getPos lexbuf) )
and action_15 lexbuf = (
 Parser.LE (getPos lexbuf) )
and action_14 lexbuf = (
 Parser.GE (getPos lexbuf) )
and action_13 lexbuf = (
 Parser.LT (getPos lexbuf) )
and action_12 lexbuf = (
 Parser.GT (getPos lexbuf) )
and action_11 lexbuf = (
 Parser.NE (getPos lexbuf) )
and action_10 lexbuf = (
 Parser.EQ (getPos lexbuf) )
and action_9 lexbuf = (
 Parser.ASSIGN (getPos lexbuf) )
and action_8 lexbuf = (
 Parser.SWAP (getPos lexbuf) )
and action_7 lexbuf = (
 Parser.LARR (getPos lexbuf) )
and action_6 lexbuf = (
 Parser.RARR (getPos lexbuf) )
and action_5 lexbuf = (
 keyword (getLexeme lexbuf, getPos lexbuf) )
and action_4 lexbuf = (
 Parser.INT (getLexeme lexbuf, getPos lexbuf) )
and action_3 lexbuf = (
 currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf )
and action_2 lexbuf = (
 Token lexbuf )
and action_1 lexbuf = (
 Token lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_20 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_20 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_15 lexbuf
 else case currChar of
    #"\t" => state_3 lexbuf
 |  #"\r" => state_3 lexbuf
 |  #" " => state_3 lexbuf
 |  #"\n" => action_3 lexbuf
 |  #"\f" => action_3 lexbuf
 |  #"~" => state_25 lexbuf
 |  #"|" => action_29 lexbuf
 |  #"^" => state_23 lexbuf
 |  #"]" => action_34 lexbuf
 |  #"[" => action_33 lexbuf
 |  #">" => state_19 lexbuf
 |  #"=" => state_18 lexbuf
 |  #"<" => state_17 lexbuf
 |  #":" => state_16 lexbuf
 |  #"/" => state_14 lexbuf
 |  #"-" => state_13 lexbuf
 |  #"," => action_35 lexbuf
 |  #"+" => state_11 lexbuf
 |  #"*" => action_18 lexbuf
 |  #")" => action_32 lexbuf
 |  #"(" => action_31 lexbuf
 |  #"&" => action_30 lexbuf
 |  #"%" => action_20 lexbuf
 |  #"!" => state_5 lexbuf
 |  #"\^@" => action_37 lexbuf
 |  _ => action_38 lexbuf
 end)
and state_3 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_47 lexbuf
 |  #"\r" => state_47 lexbuf
 |  #" " => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_38);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_11 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_11 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_16);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_24 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_13 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_17);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_6 lexbuf
 |  #"=" => action_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_19);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_40 lexbuf
 |  #"*" => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_26 lexbuf
 else backtrack lexbuf
 end)
and state_16 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_36);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_9 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_13);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_15 lexbuf
 |  #"<" => state_34 lexbuf
 |  #"-" => state_33 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_38);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_10 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_12);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => state_30 lexbuf
 |  #"=" => action_14 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_20 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_28 lexbuf
 else backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_21);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_26 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_38);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_26 lexbuf
 else backtrack lexbuf
 end)
and state_26 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_26 lexbuf
 else backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_5);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_28 lexbuf
 else backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_28 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_33 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_8 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_34 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_39 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => state_41 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_39 lexbuf
 end)
and state_40 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_1);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_40 lexbuf
 end)
and state_41 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_2 lexbuf
 |  #"\^@" => backtrack lexbuf
 |  _ => state_39 lexbuf
 end)
and state_47 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_0);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_47 lexbuf
 |  #"\r" => state_47 lexbuf
 |  #" " => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_38, action_37, action_36, action_35, action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
