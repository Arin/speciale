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
  fun doArg arg = RIL.Read (RIL.Var arg, #2 arg)
  and doArgs [] = RIL.Skip
    | doArgs [arg] = doArg (arg)
    | doArgs (arg::args) = RIL.Semi (doArg (arg), doArgs args, #2 arg)
  and doExpr (RML.ExprVar var) =
    RIL.Write (RIL.Var var, #2 var)
  and doFun (RML.FunDef (id, args, body)) =
    if not (addSymb (makeFun (#1 id)))
    then raise Error ("Duplicate function declaration at", #2 id)
    else
      let val arguments = doArgs args
      in
    (RIL.Begin (id),
    RIL.Semi (arguments,
              doExpr body,
              #2 id),
    RIL.End (id))
    end
  and doProg (funcs : RML.Prog) = ComRIL.compile (map doFun
    funcs)
end
