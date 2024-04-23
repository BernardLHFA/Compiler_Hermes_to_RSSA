mosmlc -c Hermes.sml
mosmlc -c Data.sml
mosmlyac -v HermesParser.grm
mosmlc -c HermesParser.sig
mosmlc -c HermesParser.sml
mosmllex HermesLexer.lex
mosmlc -c HermesLexer.sml
mosmlc -c HermesTypes.sig
mosmlc -c HermesTypes.sml
mosmlc -c Types.sig
mosmlc -c Types.sml
mosmlc -c BigInt.sml
mosmlc -c HermesInt.sig
mosmlc -c HermesInt.sml
mosmlc -c Interpreter.sig
mosmlc -c Interpreter.sml 
mosmlc -c Compiler.sig
mosmlc -c Compiler.sml
mosmlc -o main MainComp.sml
