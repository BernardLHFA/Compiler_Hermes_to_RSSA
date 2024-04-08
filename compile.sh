mosmlc -c Data.sml
mosmlyac -v Parser.grm
mosmlc -c Parser.sig
mosmlc -c Parser.sml
mosmllex Lexer.lex
mosmlc -c Lexer.sml
mosmlc -c Types.sig
mosmlc -c Types.sml
mosmlc -c BigInt.sml
mosmlc -o hi Main.sml

