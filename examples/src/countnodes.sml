fun countnodes tl = c_toplvl tl

and c_toplvl (tl : toplvl) : int =
  case tl of
    DefineValues (ls_sym, e) =>
    (c_list_sym ls_sym) + (c_expr e)
  | DefineSyntaxes (ls_sym, e) =>
    (c_list_sym ls_sym) + (c_expr e)
  | BeginTop (ls_toplvl) =>
    (c_list_toplvl ls_toplvl)
  | Expression e =>
    c_expr e

and c_expr (e : expr) : int =
  case e of
    VARREF s => 1 + 1
  | Lambda (fmls, ls_expr) =>
    1 + (c_formals fmls) + (c_list_expr ls_expr)
  | CaseLambda cs => c_lambda_case cs
  | If (tst, thn, els) =>
    1 + (c_expr tst) + (c_expr thn) + (c_expr els)
  | Begin ls =>
    1 + c_list_expr ls
 | Begin0 (e, ls) =>
    1 + c_expr e + c_list_expr ls
  | LetValues (lbind, ls_expr) =>
    1 + (c_lvbind lbind) + (c_list_expr ls_expr)
  | LetrecValues (lbind, ls_expr) =>
    1 + (c_lvbind lbind) + (c_list_expr ls_expr)
  | SetBang (s, e) =>
    1 + 1 + (c_expr e)
  | Quote d =>
    1 + (c_datum d)
  | QuoteSyntax d =>
    1 + (c_datum d)
  | QuoteSyntaxLocal d =>
    1 + (c_datum d)
  | WithContinuationMark (a, b, c) =>
    1 + (c_expr a) + (c_expr b) + (c_expr c)
  | App (e, ls_e) =>
    1 + (c_expr e) + (c_list_expr ls_e)
  | Top s => 1 + 1
  | VariableReference s => 2
  | VariableReferenceTop s => 2
  | VariableReferenceNull => 1

and c_lvbind (l : lvbind) =
  case l of
    CONSLVBIND (ls_s, e, rst) =>
    1 + (c_list_sym ls_s) + (c_expr e) + (c_lvbind rst)
  | NULLLVBIND => 1

and c_lambda_case (l : lambda_case) =
  case l of
    CONSLAMBDACASE (fmls, ls, rst) =>
    1 + (c_formals fmls) + (c_list_expr ls) + (c_lambda_case rst)
  | NULLLAMBDACASE => 1

and c_datum (d : datum) =
  case d of
    INTLIT i => 2

and c_formals (f : formals) =
  case f of
    F1 ls => 1 + (c_list_sym ls)
  | F2 (ls, s) => 1 + 1 + (c_list_sym ls)
  | F3 s => 1 + 1

and c_list_toplvl (ls : list_toplvl) =
  case ls of
    CONSTOPLVL (t, ts) =>
    1 + (c_toplvl t) + (c_list_toplvl ts)
  | NULLTOPLVL => 1

and c_list_expr (ls : list_expr) =
  case ls of
    CONSEXPR (e, es) =>
    1 + (c_expr e) + (c_list_expr es)
  | NULLEXPR => 1

and c_list_sym (ls : list_sym) =
  case ls of
    CONSSYM (s, ss) =>
    1 + 1 + (c_list_sym ss)
  | NULLSYM => 1

(* --------------------------------------------------------------------------- *)

fun par_countnodes tl = c_toplvl_par 0 tl

and c_toplvl_par (height : int) (tl : toplvl) : int =
  case tl of
    DefineValues (ls_sym, e) =>
    (c_list_sym ls_sym) + (c_expr e)
  | DefineSyntaxes (ls_sym, e) =>
    (c_list_sym ls_sym) + (c_expr e)
  | BeginTop (ls_toplvl) =>
    (c_list_toplvl_par height ls_toplvl)
  | Expression e =>
    c_expr e

and c_list_toplvl_par (height : int) (ls : list_toplvl) =
  if height > 8 then c_list_toplvl ls else
  case ls of
    CONSTOPLVL (t, ts) =>
    let
      val (a,b) = ForkJoin.par ( fn _ => (c_toplvl_par (height+1) t)
                               , fn _ => (c_list_toplvl_par height ts))
    in
      1 + a + b
    end
  | NULLTOPLVL => 0
