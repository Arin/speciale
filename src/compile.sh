mosmllex Lexer.lex
mosmlyac -v Parser.grm
mosmlc -c RML.sml
mosmlc -c Parser.sig Parser.sml
mosmlc -c Lexer.sml
mosmlc ComRIL.sml
mosmlc Compiler.sml
mosmlc ComRML.sml -o ComRML
