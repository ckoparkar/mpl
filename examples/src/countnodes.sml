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

fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) =
        ForkJoin.par (fn _ => ForkJoin.par (a, b),
                      fn _ => ForkJoin.par (c, d))
    in
      (ar, br, cr, dr)
    end

fun par3 (a, b, c) =
    let
      val ((ar, br), cr) =
        ForkJoin.par (fn _ => ForkJoin.par (a, b),
                      c)
    in
      (ar, br, cr)
    end

fun par_countnodes tl = par_c_toplvl 0 tl

and par_c_toplvl depth (tl : toplvl) : int =
  case tl of
    DefineValues (ls_sym, e) =>
    (par_c_list_sym (depth + 1) ls_sym) + (par_c_expr (depth + 1) e)
  | DefineSyntaxes (ls_sym, e) =>
    (par_c_list_sym (depth + 1) ls_sym) + (par_c_expr (depth + 1) e)
  | BeginTop (ls_toplvl) =>
    (par_c_list_toplvl (depth + 1) ls_toplvl)
  | Expression e =>
    par_c_expr (depth + 1) e

and par_c_expr depth (e : expr) : int =
  if (depth > 10) then c_expr e else
  case e of
    VARREF s => 1 + 1
  | Lambda (fmls, ls_expr) =>
    1 + (par_c_formals (depth + 1) fmls) + (par_c_list_expr (depth + 1) ls_expr)
  | CaseLambda cs => c_lambda_case cs
  | If (tst, thn, els) =>
    let
      val (a,b,c) = par3( fn _ => (par_c_expr (depth + 1) tst)
                        , fn _ => (par_c_expr (depth + 1) thn)
                        , fn _ => (par_c_expr (depth + 1) els)
                        )
    in 1 + a + b + c
    end
  | Begin ls =>
    1 + par_c_list_expr (depth + 1) ls
 | Begin0 (e, ls) =>
    1 + par_c_expr (depth + 1) e + par_c_list_expr (depth + 1) ls
  | LetValues (lbind, ls_expr) =>
    1 + (par_c_lvbind (depth + 1) lbind) + (par_c_list_expr (depth + 1) ls_expr)
  | LetrecValues (lbind, ls_expr) =>
    1 + (par_c_lvbind (depth + 1) lbind) + (par_c_list_expr (depth + 1) ls_expr)
  | SetBang (s, e) =>
    1 + 1 + (par_c_expr (depth + 1) e)
  | Quote d =>
    1 + (par_c_datum (depth + 1) d)
  | QuoteSyntax d =>
    1 + (par_c_datum (depth + 1) d)
  | QuoteSyntaxLocal d =>
    1 + (par_c_datum (depth + 1) d)
  | WithContinuationMark (a, b, c) =>
    let
      val (a,b,c) = par3( fn _ => (par_c_expr (depth + 1) a)
                        , fn _ => (par_c_expr (depth + 1) b)
                        , fn _ => (par_c_expr (depth + 1) c)
                        )
    in 1 + a + b + c
    end
  | App (e, ls_e) =>
    let
      val (a,b) = ForkJoin.par( fn _ => (par_c_expr (depth + 1) e)
                              , fn _ => (par_c_list_expr (depth + 1) ls_e)
                              )
    in 1 + a + b
    end
  | Top s => 1 + 1
  | VariableReference s => 2
  | VariableReferenceTop s => 2
  | VariableReferenceNull => 1

and par_c_lvbind depth (l : lvbind) =
  case l of
    CONSLVBIND (ls_s, e, rst) =>
    1 +
    (par_c_list_sym (depth + 1) ls_s) +
    (par_c_expr (depth + 1) e) +
    (par_c_lvbind (depth + 1) rst)
  | NULLLVBIND => 1

and par_c_lambda_case depth (l : lambda_case) =
  case l of
    CONSLAMBDACASE (fmls, ls, rst) =>
    1 +
    (par_c_formals (depth + 1) fmls) +
    (par_c_list_expr (depth + 1) ls) +
    (par_c_lambda_case (depth + 1) rst)
  | NULLLAMBDACASE => 1

and par_c_datum depth (d : datum) =
  case d of
    INTLIT i => 2

and par_c_formals depth (f : formals) =
  case f of
    F1 ls => 1 + (par_c_list_sym (depth + 1) ls)
  | F2 (ls, s) => 1 + 1 + (par_c_list_sym (depth + 1) ls)
  | F3 s => 1 + 1

and par_c_list_toplvl depth (ls : list_toplvl) =
  case ls of
    CONSTOPLVL (t, ts) =>
    1 + (par_c_toplvl (depth + 1) t) + (par_c_list_toplvl (depth + 1) ts)
  | NULLTOPLVL => 1

and par_c_list_expr depth (ls : list_expr) =
  case ls of
    CONSEXPR (e, es) =>
    1 + (par_c_expr (depth + 1) e) + (par_c_list_expr (depth + 1) es)
  | NULLEXPR => 1

and par_c_list_sym depth (ls : list_sym) =
  case ls of
    CONSSYM (s, ss) =>
    1 + 1 + (par_c_list_sym (depth + 1) ss)
  | NULLSYM => 1
