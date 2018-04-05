structure Compiler = 
  struct

  exception Error of string*(int*int)

  (* Contains information on variables and functions - 
   * Variables are written as var_varname
   * Functions are written as fun_funname *)
  val symTab = ref []

  (* Probably needs to be rewritten, to account for local vars, etc. *)
  local
    fun addSymb' x [] = ((symTab := x :: !symTab); true)
      | addSymb' x (y :: ys) = if x=y 
                               then false
                               else (addSymb' x ys)
  in
    fun addSymb x = addSymb' x (!symTab)
  end

  fun makeVar x = "var_" ^ x

  (* Ensures the main function doesn't have its name changed 
   * May be changed in next version, as formal sml doesn't have a main funct *)
  fun makeFun x = if x = "main"
                  then x
                  else "fun_" ^ x
  (* TODO: Add args
   * fun doArgs x =
   *)

  fun doExpr (RML.ExprVar var) =
    RIL.Var var
  and doFun (RML.FunDef (id, args, body)) = 
    if not (addSymb (makeFun (#1 id)))
    then raise Error ("Duplicate function declaration at", #2 id)
    else (RIL.Begin (id), RIL.Write (doExpr body, (1,1)), RIL.End (id)) 
  and doProg (funcs : RML.Prog) = 
    (TextIO.output (TextIO.stdOut, "Doing prog...\n"); ComRIL.compile (map doFun
    funcs))
end
