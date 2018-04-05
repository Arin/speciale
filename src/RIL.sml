structure RIL =
  struct

  (* types for abstract syntax for RIL *)

  type pos = int * int  (* position: line, column *)

  datatype lVal
    = Var of string * pos
    | Mem of string * pos

  datatype rVal
    = L of lVal
    | Const of int * pos

  datatype updateOp = Add | Sub | XorWith

  datatype arithOp = Plus | Minus | Xor | And | Or | ShiftL | ShiftR

  datatype relOp = Equal | Less | Greater | Neq | Leq | Geq | IfAnd | IfNand

  type cond = lVal * relOp * rVal

  datatype entry
    = Begin of string * pos
    | Label of string * pos
    | Join of cond * string * string * pos

  datatype exit
    = End of string * pos
    | Goto of string * pos
    | Split of cond * string * string * pos

  datatype instruct
    = Update of lVal * updateOp * rVal * arithOp * rVal * pos
    | Swap of lVal * lVal * pos
    | Call of string * pos
    | Uncall of string * pos
    | Assert of cond list * pos
    | Read of lVal * pos
    | Write of lVal * pos
    | Test of string * rVal list * pos
    | IncludeC of string * pos
    | Skip
    | Semi of instruct * instruct * pos
    | Timer

  type block = entry * instruct * exit

  type prog = block list


  fun showlVal l =
    case l of
      Var (x,p) => x
    | Mem (x,p) => "M["^ x ^"]"

  fun showrVal r =
    case r of
      L l => showlVal l
    | Const (n, pos) => Int.toString n

  fun showUpdateOp Add = " += "
    | showUpdateOp Sub = " -= "
    | showUpdateOp XorWith = " ^= "

  fun showArithOp Plus = " + "
    | showArithOp Minus = " - "
    | showArithOp Xor = " ^ "
    | showArithOp And = " & "
    | showArithOp Or  = " | "
    | showArithOp ShiftL = " << "
    | showArithOp ShiftR = " >> "

  fun showRelOp Equal = " == "
    | showRelOp Less = " < "
    | showRelOp Greater = " > "
    | showRelOp Neq = " != "
    | showRelOp Leq = " <= "
    | showRelOp Geq = " >= "
    | showRelOp ifAnd = " & "

  fun showCond (l, rop, r) = showlVal l ^ showRelOp rop ^ showrVal r

  fun showConds [c] = showCond c
    | showConds (c :: cs) = showCond c ^ " && " ^ showConds cs
    | showConds _ = raise Domain

  fun showEntry (Begin (l, p)) = "begin " ^ l
    | showEntry (Label (l, p)) = l ^ " <-- "
    | showEntry (Join (c, l1, l2, p))
       =  l1 ^ "; " ^ l2 ^ " <-- " ^ showCond c

  fun showExit (End (l, p)) = "end " ^ l
    | showExit (Goto (l, p)) = " --> " ^ l
    | showExit (Split (c, l1, l2, p))
       = showCond c ^ " --> " ^ l1 ^ "; " ^ l2

  fun showInstruct (Update (l, uop, r1, aop, r2, pos))
       = showlVal l ^ showUpdateOp uop ^ showrVal r1
         ^ showArithOp aop ^ showrVal r2
    | showInstruct (Swap (l1, l2, pos))
       = showlVal l1 ^ " <-> " ^ showlVal l2
    | showInstruct (Call (l, pos)) = "call " ^ l
    | showInstruct (Uncall (l, pos)) = "uncall " ^ l
    | showInstruct (Assert (cs, pos)) = "assert " ^ showConds cs
    | showInstruct (Read (l, pos)) = "read " ^ showlVal l
    | showInstruct (Write (l, pos)) = "write " ^ showlVal l
    | showInstruct (Test (s, rvs, pos))
       = "test " ^ s ^ " " ^
         String.concat (List.map (fn r => showrVal r ^ " ") rvs)
    | showInstruct (IncludeC (s, pos)) = "includeC " ^ s
    | showInstruct Skip = "skip"
    | showInstruct Timer = "timer"
    | showInstruct (Semi (i1, i2, pos))
       = showInstruct i1 ^ ";\n" ^ showInstruct i2

  fun showBlock (en, is, ex)
       = showEntry en ^ "\n" ^ showInstruct is ^ "\n" ^ showExit ex ^ "\n\n"

  fun showProg bs = String.concat (List.map showBlock bs)

end
