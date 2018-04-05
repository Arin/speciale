{
  open Lexing;

  exception LexicalError of string * (int * int)

  val currentLine = ref 1
  val lineStartPos = ref [0]

  fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
                      (!currentLine) (!lineStartPos)

  and getLineCol pos line (p1::ps) = if pos >= p1 then (line, pos-p1)
                                     else getLineCol pos (line-1) ps
                    | getLineCol pos line [] = raise LexicalError ("",(0,0))

  fun lexerError lexbuf s = raise LexicalError (s, getPos lexbuf)

  fun keyword (s, pos) =
    case s of
      "fun" => Parser.FUNDEF pos
    | _     => Parser.ID (s, pos)
}

rule Token = parse
    [` ` `\t` `\r`]+ { Token lexbuf } (* whitespace *)
  | "//" [^`\n`]*  { Token lexbuf } (* comment (singleline) *)
  | [`\n` `\012`]    { currentLine  := !currentLine+1;
                       lineStartPos := getLexemeStart lexbuf :: !lineStartPos;
                       Token lexbuf } (* newlines *)
  | [`0`-`9`]+       { case Int.fromString (getLexeme lexbuf) of
                            NONE   => lexerError lexbuf "Bad integer"
                          | SOME i => Parser.NUM (i, getPos lexbuf) }
  | [`a`-`z``A`-`Z`] [`a`-`z``A`-`Z``0`-`9``_`]*
                     { keyword (getLexeme lexbuf, getPos lexbuf) }
  | `"` ([` ` `!` `#`-`&` `(`-`[` `]`-`~`] | `\`[` `-`~`])* `"`
                     { Parser.STRINGCONST
                         ((case String.fromCString (getLexeme lexbuf) of
                            NONE => lexerError lexbuf "Bad string constant"
                          | SOME s => String.substring(s,1,String.size s - 2)),
                          getPos lexbuf) }
  | `=`              { Parser.ASSIGN (getPos lexbuf) }
  | `|`              { Parser.PATTERN (getPos lexbuf) }
  | eof              { Parser.EOF (getPos lexbuf) }
  | _                { lexerError lexbuf "Illegal symbol in input" }
;
