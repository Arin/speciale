structure ComRIL =
struct

  exception Error of string*(int*int)

  val counter = ref 0
  fun gen () = (counter := !counter + 1; "_" ^ Int.toString (!counter))

  fun testOut s = () (* TextIO.output (TextIO.stdErr, s ^ "\n") *)

  (* memory *)
  val memSize = "0x10000000" (* 2^28 *)
  val memInit = "  uint Mem[" ^ memSize ^ "];\n\n"

  (* execution counters *)
  val counterInit =
      "  uint iCount = 0;\n  uint mCount = 0; int timer = 1;\n\n" ^
      "  void count(int i, int m) {if (timer) {iCount += i; mCount += m;}}\n\n"

  (* return stack *)
  val stackInit =
      "  void *stack[10000];\n  int stackPointer = 0;\n\n"

  val allInit = memInit ^ counterInit ^ stackInit
(*
  fun count i m =
    "iCount += " ^ Int.toString i ^ ";\n" ^
    "mCount += " ^ Int.toString m ^ ";\n"
*)

 (* variable list *)
  val env = ref []

  local
    fun addVar' x [] = env := x :: !env
      | addVar' x (y :: ys) = if x=y then ()
                              else addVar' x ys
  in
    fun addVar x = addVar' x (!env)
  end

  (* get or put values in memory or variables *)

  fun getLval (RIL.Var (x, pos)) =
        (addVar x; "var_" ^ x)
    | getLval (RIL.Mem (x, pos)) =
        (addVar x;  "(count(0,1), Mem[var_" ^ x ^"])")

  fun putLval (RIL.Var (x, pos)) n =
        (addVar x; "  var_" ^ x ^ " = (") ^ n
    | putLval (RIL.Mem (x, pos)) n =
        (addVar x; "  Mem[var_" ^ x ^ "] = (count(0,1), ") ^ n

  fun getRval (RIL.L lv) = getLval lv
    | getRval (RIL.Const (n,p)) = Int.toString n

  fun updateLval RIL.Add lv n = putLval lv (getLval lv ^ " + " ^ n) ^");"
    | updateLval RIL.Sub lv n = putLval lv (getLval lv ^ " - " ^ n) ^");"
    | updateLval RIL.XorWith lv n = putLval lv (getLval lv ^ " ^ " ^ n) ^");"

  (* arithmetic operations *)
  fun doArith RIL.Plus m n = "(" ^ m ^ " + " ^ n ^ ")"
    | doArith RIL.Minus m n = "(" ^ m ^ " - " ^ n ^ ")"
    | doArith RIL.Xor m n = "(" ^ m ^ " ^ " ^ n ^ ")"
    | doArith RIL.And m n = "(" ^ m ^ " & " ^ n ^ ")"
    | doArith RIL.Or m n = "(" ^ m ^ " | " ^ n ^ ")"
    | doArith RIL.ShiftL m n = "(" ^ m ^ " << " ^ n ^ ")"
    | doArith RIL.ShiftR m n = "(" ^ m ^ " >> " ^ n ^ ")"

  (* check overlapping left/right-hand sides *)
  fun checkOverlap lv (RIL.Const _) pos = ()
    | checkOverlap (RIL.Var (x,_)) (RIL.L (RIL.Var (y,_))) pos =
        if x=y then raise Error ("Overlapping left/right-hand sides", pos)
        else ()
    | checkOverlap (RIL.Var (x,_)) (RIL.L (RIL.Mem (y,_))) pos =
        if x=y then raise Error ("Overlapping left/right-hand sides", pos)
        else ()
    | checkOverlap (RIL.Mem (x,_)) (RIL.L (RIL.Var (y,_))) pos =
        if x=y then raise Error ("Overlapping left/right-hand sides", pos)
        else ()
    | checkOverlap (RIL.Mem _) (RIL.L (RIL.Mem _)) pos =
        raise Error ("Overlapping left/right-hand sides", pos)

  (* compile condition *)
  fun checkCond (lv, RIL.Equal, rv) = getLval lv ^ " == " ^ getRval rv
    | checkCond (lv, RIL.Less, rv) = getLval lv ^ " < " ^ getRval rv
    | checkCond (lv, RIL.Greater, rv) = getLval lv ^ " > " ^ getRval rv
    | checkCond (lv, RIL.Neq, rv) = getLval lv ^ " != " ^ getRval rv
    | checkCond (lv, RIL.Leq, rv) = getLval lv ^ " <= " ^ getRval rv
    | checkCond (lv, RIL.Geq, rv) = getLval lv ^ " >= " ^ getRval rv
    | checkCond (lv, RIL.IfAnd, rv) =
          "(" ^ getLval lv ^ " & " ^ getRval rv ^ ")"
    | checkCond (lv, RIL.IfNand, rv) =
          "(" ^ getLval lv ^ " & " ^ getRval rv ^ ") == 0"

  (* make labels *)
  fun directLabel l = "Label_" ^ l

  fun invertLabel l = if String.isPrefix "_" l
                      then "Label_" ^ String.extract(l,1,NONE)
                      else "Label__" ^ l

  (* compile instruction *)
  fun doInst (RIL.Update (lv, upd, rv1, aop, rv2, pos)) =
        let
          val v1 = getRval rv1
          val v2 = getRval rv2
          val w = doArith aop v1 v2
        in
          checkOverlap lv rv1 pos;
          checkOverlap lv rv2 pos;
          "count(1, 0);\n" ^ updateLval upd lv w ^ "\n"
        end
    | doInst (RIL.Swap (lv1, lv2,pos)) =
        let
          val v1 = getLval lv1
          val v2 = getLval lv2
        in
          checkOverlap lv1 (RIL.L lv2) pos;
          "  tmp = " ^ v1 ^ ";\n" ^ 
          putLval lv1 v2 ^ ");\n" ^
          putLval lv2 "tmp);\n"
        end
    | doInst (RIL.Read (lv,pos)) =
        "  if (" ^ getLval lv ^ " != 0 || scanf(\"%d\\n\",&" ^ getLval lv ^ ") != 1)" ^
        "  { fprintf(stderr,\"Read failure at line "
        ^ Int.toString (#1 pos) ^ "\\n\");\n" ^
        "  goto programEnd;\n}\n"
    | doInst (RIL.Write (lv,pos)) =
        "printf(\"%u\\n\", " ^ getLval lv ^ ");\n" ^
        "count(1,0);\n" ^ putLval lv "0" ^ ");\n"
    | doInst (RIL.Test (str, rvs,pos)) =
        "fprintf(stderr,\"" ^ String.toCString str ^ "\"" ^
        (if rvs = [] then ""
         else String.concat (List.map (fn r => ", " ^ getRval r) rvs))
        ^ ");\n"
    | doInst (RIL.IncludeC (str, pos)) =
        "\n" ^ str ^ "\n"
    | doInst (RIL.Assert (conds, pos)) =
        let
          val tests = List.map (fn c => checkCond c ^ " && ") conds
          val test = String.concat tests ^ "1"
        in
          "  if (!(" ^ test ^
          ")){\n    fprintf(stderr,\"\\nAssertion failure at line " ^
         Int.toString (#1 pos) ^ "\\n\");\n" ^
         "  writeVars(); goto programEnd;\n}\n"
        end
    | doInst (RIL.Call (lab,pos)) =
        let
          val returnLabel = "Return_" ^ Int.toString (#1 pos) ^ gen()
        in
          "  count(2,0);\n " ^
          "  stack[stackPointer++] = &&" ^ returnLabel ^ ";\n" ^
          "  goto " ^ directLabel lab ^ ";\n  " ^ returnLabel ^ ":\n"
        end
    | doInst (RIL.Uncall (lab,pos)) =
        let
          val returnLabel = "Return_" ^ Int.toString (#1 pos) ^ gen()
        in
          "  count(2,0);\n " ^
          "  stack[stackPointer++] = &&" ^ returnLabel ^ ";\n" ^
          "  goto " ^ invertLabel lab ^ ";\n  " ^ returnLabel ^ ":\n"
        end
    | doInst (RIL.Semi (i1, i2, pos)) =
        doInst i1 ^ doInst i2
    | doInst RIL.Skip = ""
    | doInst RIL.Timer = "  timer = 1 - timer;\n"

  (* execute entry point *)

  and doEntry  [] = ""
    | doEntry  ((RIL.Label (l1, p1), inst, ex) :: bs) =
        "Label_" ^ l1 ^ ":\n" ^ doInst inst ^ doExit ex ^
        doEntry bs
    | doEntry ((RIL.Join (c, l1, l2, p1), inst, ex) :: bs) =
        "Label_" ^  l1 ^ ":\n" ^
        "  if (count(1,0), !(" ^ checkCond c ^
        ")){\n    fprintf(stderr,\"\\nJoin failure at line " ^
         Int.toString (#1 p1) ^ "\\n\");\n" ^
         "  writeVars(); goto programEnd;\n}\n" ^
        doInst inst ^ doExit ex ^
        "Label_" ^  l2 ^ ":\n" ^
        "  if (count(1,0), " ^ checkCond c ^
        "){\n    fprintf(stderr,\"\\nJoin failure at line " ^
         Int.toString (#1 p1) ^ "\\n\");\n" ^
         "  writeVars(); goto programEnd;\n}\n" ^
        doInst inst ^ doExit ex ^
        doEntry bs
    | doEntry  ((RIL.Begin (l1, p1), inst, ex) :: bs) =
        "Label_" ^ l1 ^ ":\n" ^
        (* "fprintf(stderr,\"" ^ l1 ^ "\\n\");\n" ^ *)
        doInst inst ^ doExit ex ^ doEntry bs

  (* execute exit point *)
  and doExit (RIL.End (lab, pos)) =
        "  goto *stack[--stackPointer];\n\n"
    | doExit (RIL.Goto (lab,pos)) =
        "  goto Label_" ^ lab ^ ";\n\n"
    | doExit (RIL.Split (c, labt, labf, pos)) =
        "  if (count(1,0), " ^ checkCond c ^ ") " ^
        "goto Label_" ^ labt ^ "; " ^
        "else goto Label_" ^ labf ^ ";\n\n"


  fun invertUpdate RIL.Add = RIL.Sub
    | invertUpdate RIL.Sub = RIL.Add
    | invertUpdate RIL.XorWith = RIL.XorWith

  fun invertInst (RIL.Update (lv, upd, rv1, aop, rv2, pos)) =
        RIL.Update (lv, invertUpdate upd, rv1, aop, rv2, pos)
    | invertInst (RIL.Read lvpos) = RIL.Write lvpos
    | invertInst (RIL.Write lvpos) = RIL.Read lvpos
    | invertInst (RIL.Test lvpos) = RIL.Test lvpos
    | invertInst (RIL.Semi (i1, i2, pos)) =
        RIL.Semi (invertInst i2, invertInst i1, pos)
    | invertInst (RIL.Call (l, pos)) = 
        RIL.Call ("_" ^ l, pos)
    | invertInst (RIL.Uncall (l, pos)) = 
        RIL.Uncall ("_" ^ l, pos)
    | invertInst (RIL.Assert (conds,(line, pos))) =
        RIL.Assert (conds,(~line, pos))
    | invertInst other = other

  fun invertEntry (RIL.Begin (lab,pos)) = RIL.End ("_" ^ lab,pos)
    | invertEntry (RIL.Label (lab,pos)) = RIL.Goto ("_" ^ lab,pos)
    | invertEntry (RIL.Join (cond,labt,labf,pos)) =
        RIL.Split (cond, "_" ^ labt, "_" ^ labf, pos)

  fun invertExit (RIL.End (lab,pos)) = RIL.Begin ("_" ^ lab,pos)
    | invertExit (RIL.Goto (lab,pos)) = RIL.Label  ("_" ^ lab,pos)
    | invertExit (RIL.Split (cond,labt,labf,pos)) =
        RIL.Join (cond, "_" ^ labt, "_" ^ labf, pos)

  fun invertBlock (en,inst,ex) =
        (invertExit ex, invertInst inst, invertEntry en)

  fun compile pgm =
    let
      val pgmInv = (TextIO.output (TextIO.stdout, "Inverting blocks...");map
      invertBlock pgm)
      val blocks = (TextIO.output (TextIO.stdout, "Doing normal blocks...); "/*  blocks */\n\n" ^ doEntry pgm ^
                   "/*  inverse blocks */\n\n" ^ doEntry pgmInv)
    in
      (TextIO.output (TextIO.stdOut, "Starting compile process...";
       "#include <stdio.h>\n#include <stdlib.h>\n\n" ^
      allInit ^ "\n" ^
      "int main(int ac, int* av)\n{\n" ^
      "  uint tmp;\n" ^
      "  int A, B, C, D, E, F, G, H;\n" ^
      String.concat (List.map (fn x => "  uint var_" ^ x ^ " = 0;\n") (!env)) ^
      "\n  void writeVars() {\n" ^
      String.concat
        (List.map
           (fn x =>
             "    printf(\"var_" ^ x ^ " = \\%u\\n\",var_" ^x ^ ");\n") (!env)) ^
      "  }\n  stack[stackPointer++] = &&programEnd;\n" ^
      "  goto Label_main;\n\nprogramEnd:\n" ^
      "  fprintf(stderr,\"\\nInstructions: %u\\nMemory accesses: %u\\n\"," ^
      "iCount, mCount);\n\n  exit(0);\n\n" ^
      blocks ^ "\n}")
    end
end
