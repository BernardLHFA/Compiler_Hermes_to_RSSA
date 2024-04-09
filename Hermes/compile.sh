mosmlc -c Hermes.sml
mosmlyac -v HermesParser.grm
mosmlc -c HermesParser.sig
mosmlc -c HermesParser.sml
mosmllex HermesLexer.lex
mosmlc -c HermesLexer.sml
mosmlc -c HermesTypes.sig
mosmlc -c HermesTypes.sml
mosmlc -c BigInt.sml
mosmlc -c HermesInt.sig
mosmlc -c HermesInt.sml
mosmlc -o hi hi.sml
mosmlc -c HermesReify.sml
mosmlc -o hr hr.sml
mosmlc -c HermesPE.sml
mosmlc -o hpe hpe.sml
mosmlc -c x86.sml
mosmlc -c HermesCx64.sml
mosmlc -o hcX64 hcX64.sml

