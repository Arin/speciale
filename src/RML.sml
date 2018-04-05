structure RML =
  struct

  type pos = int * int (* position: (line, column) *)
  type Var = string * pos

  datatype Type =
    Int
  | Bool
  | Array of Type

  datatype Value =
    IntVal of int * pos
  | BoolVal of bool * pos
  (*| ArrayVal of Value list * Type * pos*)

  (* TODO: Add more expressions *)
  datatype Expr =
    ExprVar of Var
  | ExprVal of Value

  type Id = string * pos

  type Arg = Id

  type Args = Arg list

  (* TODO: Add LetExprs, change to datatype*)
  type Body = Expr

  datatype Func =
    FunDef of Id * Args * Body

  type Funcs = Func list

  type Prog = Funcs

(*  fun valueType (IntVal _)         = Int
    | valueType (BoolVal _)        = Bool
    (*| valueType (ArrayVal (_, tp)) = Array tp*)


  fun showValue (IntVal n)  = n
    (*| showValue (BoolVal b) = b*)
  (* TODO: Construct var tables? *)

  fun writeArgs [] = ""
    | writeArgs (arg :: args) =   ""

  fun writeBody (e) =
    case e of
      ExprVar (v, p)   => ""
    | ExprVal (IntVal (v, p))   =>
        RIL.showInstruct (RIL.Update
                            ((RIL.Var ("result", p)),
                             RIL.Add, RIL.Const (v, p),
                             RIL.Plus,
                             RIL.Const(0, p),
                             p))


  fun writeFunDef (FunDef ((id, pos), args, body)) =
    (RIL.showEntry (RIL.Begin (id, pos))) (*^ (writeArgs args)*) ^ (writeBody body)
*)
end
