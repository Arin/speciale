
prog ::= funcs <u>EOF</u>

funcs ::= func funcs | none
(typing for funcs: func list)

func ::= <u>fun</u> fundef

fundef ::= <u>id</u> args <u>=</u> body

body ::= exprs <u>|</u> fundef | exprs

exprs ::= letexprs expr | expr

letexprs ::= <u>let</u> updates <u>in</u> letexprs | none

updates ::= <u>var</u> <u>=</u> expr

expr ::= <u>var</u> | expr $\oplus$ expr | (expr)

args ::= args' | () | args' <u>?</u> bexpr

args' ::= arg args' | arg

arg ::= <u>var</u> | typing <u>var</u> | <u>const</u> | _
