val cons_cost = 1
val scalar_cost = 0
val tag_cost = 1

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
    tag_cost + (c_formals fmls) + (c_list_expr ls_expr)
  | CaseLambda cs => c_lambda_case cs
  | If (tst, thn, els) =>
    tag_cost + (c_expr tst) + (c_expr thn) + (c_expr els)
  | Begin ls =>
    tag_cost + c_list_expr ls
 | Begin0 (e, ls) =>
    tag_cost + c_expr e + c_list_expr ls
  | LetValues (lbind, ls_expr) =>
    tag_cost + (c_lvbind lbind) + (c_list_expr ls_expr)
  | LetrecValues (lbind, ls_expr) =>
    tag_cost + (c_lvbind lbind) + (c_list_expr ls_expr)
  | SetBang (s, e) =>
    tag_cost + scalar_cost + (c_expr e)
  | Quote d =>
    tag_cost + (c_datum d)
  | QuoteSyntax d =>
    tag_cost + (c_datum d)
  | QuoteSyntaxLocal d =>
    tag_cost + (c_datum d)
  | WithContinuationMark (a, b, c) =>
    tag_cost + (c_expr a) + (c_expr b) + (c_expr c)
  | App (e, ls_e) =>
    tag_cost + (c_expr e) + (c_list_expr ls_e)
  | Top s => tag_cost + scalar_cost
  | VariableReference s => tag_cost + scalar_cost
  | VariableReferenceTop s => tag_cost + scalar_cost
  | VariableReferenceNull => tag_cost

and c_lvbind (l : lvbind) =
  case l of
    CONSLVBIND (ls_s, e, rst) =>
    cons_cost + (c_list_sym ls_s) + (c_expr e) + (c_lvbind rst)
  | NULLLVBIND => 1

and c_lambda_case (l : lambda_case) =
  case l of
    CONSLAMBDACASE (fmls, ls, rst) =>
    cons_cost + (c_formals fmls) + (c_list_expr ls) + (c_lambda_case rst)
  | NULLLAMBDACASE => 1

and c_datum (d : datum) =
  case d of
    INTLIT i => cons_cost + scalar_cost

and c_formals (f : formals) =
  case f of
    F1 ls => tag_cost + (c_list_sym ls)
  | F2 (ls, s) => tag_cost + scalar_cost + (c_list_sym ls)
  | F3 s => tag_cost + scalar_cost

and c_list_toplvl (ls : list_toplvl) =
  case ls of
    CONSTOPLVL (t, ts) =>
    1 + (c_toplvl t) + (c_list_toplvl ts)
  | NULLTOPLVL => 1

and c_list_expr (ls : list_expr) =
  case ls of
    CONSEXPR (e, es) =>
    1 + (c_expr e) + (c_list_expr es)
  | NULLEXPR => tag_cost

and c_list_sym (ls : list_sym) =
  case ls of
    CONSSYM (s, ss) =>
    1 + 1 + (c_list_sym ss)
  | NULLSYM => tag_cost

(* --------------------------------------------------------------------------- *)

fun par_countnodes tl = c_toplvl_par 0 tl

and c_toplvl_par (height : int) (tl : toplvl) : int =
  case tl of
    DefineValues (ls_sym, e) =>
    tag_cost + (c_list_sym ls_sym) + (c_expr e)
  | DefineSyntaxes (ls_sym, e) =>
    tag_cost + (c_list_sym ls_sym) + (c_expr e)
  | BeginTop (ls_toplvl) =>
    tag_cost + (c_list_toplvl_par height ls_toplvl)
  | Expression e =>
    tag_cost + c_expr e

and c_list_toplvl_par (height : int) (ls : list_toplvl) =
  if height >= 9 then c_list_toplvl ls else
  case ls of
    CONSTOPLVL (t, ts) =>
    let
      val (a,b) = ForkJoin.par ( fn _ => (c_toplvl_par (height+1) t)
                               , fn _ => (c_list_toplvl_par height ts))
    in
      tag_cost + a + b
    end
  | NULLTOPLVL => tag_cost
