structure Pr = SExpPrinter
structure S = SExp
structure A = Atom

structure L = List

type sym = int

datatype toplvl
  = DefineValues of (list_sym * expr)
  | DefineSyntaxes of (list_sym * expr)
  | BeginTop of list_toplvl
  | Expression of expr

and expr
  = VARREF of sym
  | Lambda of (formals * list_expr)
  | CaseLambda of lambda_case
  | If of (expr * expr * expr)
  | Begin of list_expr
  | Begin0 of (expr * list_expr)
  | LetValues of (lvbind * list_expr)
  | LetrecValues of (lvbind * list_expr)
  | SetBang of (sym * expr)
  | Quote of datum
  | QuoteSyntax of datum
  | QuoteSyntaxLocal of datum
  | WithContinuationMark of (expr * expr * expr)
  | App of (expr * list_expr)
  | Top of sym
  | VariableReference of sym
  | VariableReferenceTop of sym
  | VariableReferenceNull

and lvbind
  = CONSLVBIND of (list_sym * expr * lvbind)
  | NULLLVBIND

and lambda_case
  = CONSLAMBDACASE of (formals * list_expr * lambda_case)
  | NULLLAMBDACASE

and datum = INTLIT of int

and formals
  = F1 of list_sym
  | F2 of (list_sym * sym)
  | F3 of sym

and list_toplvl
  = CONSTOPLVL of (toplvl * list_toplvl)
  | NULLTOPLVL

and list_expr
  = CONSEXPR of (expr * list_expr)
  | NULLEXPR

and list_sym
  = CONSSYM of (sym * list_sym)
  | NULLSYM


fun parse_toplvl (v : S.value) : toplvl =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "DefineValues"
      then
        let
          val ls_sym = parse_list_sym (L.nth (rst, 0))
          val exp = parse_expr (L.nth (rst, 1))
        in DefineValues (ls_sym, exp)
        end
      else if str_eq ss "DefineSyntaxes"
      then
        let
          val ls_sym = parse_list_sym (L.nth (rst, 0))
          val exp = parse_expr (L.nth (rst, 1))
        in DefineValues (ls_sym, exp)
        end
      else if str_eq ss "BeginTop"
      then
        let
          val ls_toplvl = parse_list_toplvl (L.nth (rst, 0))
        in BeginTop ls_toplvl
        end
      else if str_eq ss "Expression"
      then
        let
          val exp = parse_expr (L.nth (rst, 0))
        in Expression exp
        end
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse toplvl 1")
        end
    end
  | _ =>
    let
      val _ = Pr.print (TextIO.stdOut, v)
    in
      raise Fail ("cannot parse toplvl 1")
    end

and parse_expr (v : S.value) : expr =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "VARREF"
      then VARREF (parse_sym (L.nth (rst, 0)))
      else if str_eq ss "Lambda"
      then
        let
          val fmls = parse_formals (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
        in Lambda (fmls, ls_expr)
        end
      else if str_eq ss "CaseLambda"
      then CaseLambda (parse_lambda_case (L.nth (rst, 0)))
      else if str_eq ss "If"
      then
        let
          val tst = parse_expr (L.nth (rst, 0))
          val thn = parse_expr (L.nth (rst, 1))
          val els = parse_expr (L.nth (rst, 2))
        in
          If (tst, thn, els)
        end
      else if str_eq ss "Begin"
      then Begin (parse_list_expr (L.nth (rst, 0)))
      else if str_eq ss "Begin0"
      then
        let
          val exp = parse_expr (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
        in
          Begin0 (exp, ls_expr)
        end
      else if str_eq ss "LetValues"
      then
        let
          val lvbnd = parse_lvbind (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
        in
          LetValues (lvbnd, ls_expr)
        end
      else if str_eq ss "LetrecValues"
      then
        let
          val lvbnd = parse_lvbind (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
        in
          LetValues (lvbnd, ls_expr)
        end
      else if str_eq ss "SetBang"
      then
        let
          val sy = parse_sym (L.nth (rst, 0))
          val exp = parse_expr (L.nth (rst, 1))
        in
          SetBang (sy, exp)
        end
      else if str_eq ss "Quote"
      then
        let
          val dt = parse_datum (L.nth (rst, 0))
        in
          Quote dt
        end
      else if str_eq ss "QuoteSyntax"
      then
        let
          val dt = parse_datum (L.nth (rst, 0))
        in
          QuoteSyntax dt
        end
      else if str_eq ss "QuoteSyntaxLocal"
      then
        let
          val dt = parse_datum (L.nth (rst, 0))
        in
          QuoteSyntaxLocal dt
        end
      else if str_eq ss "WithContinuationMark"
      then
        let
          val tst = parse_expr (L.nth (rst, 0))
          val thn = parse_expr (L.nth (rst, 1))
          val els = parse_expr (L.nth (rst, 2))
        in
          WithContinuationMark (tst, thn, els)
        end
      else if str_eq ss "App"
      then
        let
          val exp = parse_expr (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
        in
          App (exp, ls_expr)
        end
      else if str_eq ss "Top"
      then Top (parse_sym (L.nth (rst, 0)))
      else if str_eq ss "VariableReference"
      then VariableReference (parse_sym (L.nth (rst, 0)))
      else if str_eq ss "VariableReferenceTop"
      then VariableReferenceTop (parse_sym (L.nth (rst, 0)))
      else if str_eq ss "VariableReferenceNull"
      then VariableReferenceNull
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse expr 1")
        end
    end

  | _ =>
    let
      val _ = Pr.print (TextIO.stdOut, v)
    in
      raise Fail ("cannot parse expr 2")
    end

and parse_sym (v : S.value) : sym =
  case v of
    S.SYMBOL s => 0
  | _ =>
    let
      val _ = Pr.print (TextIO.stdOut, v)
    in
      raise Fail ("cannot parse sym")
    end

and parse_lvbind (v : S.value) : lvbind =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "CONSLVBIND"
      then
        let
          val ls_sym = parse_list_sym (L.nth (rst, 0))
          val exp = parse_expr (L.nth (rst, 1))
          val lvbnd = parse_lvbind (L.nth (rst, 2))
        in
          CONSLVBIND (ls_sym, exp, lvbnd)
        end
      else if str_eq ss "NULLLVBIND"
      then NULLLVBIND
      else raise Fail "cannot parse lvbind"
    end
    | _ => raise Fail "cannot parse lvbind"

and parse_lambda_case (v : S.value) : lambda_case =
    case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "CONSLAMBDACASE"
      then
        let
          val fmls = parse_formals (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
          val lcase = parse_lambda_case (L.nth (rst, 2))
        in
          CONSLAMBDACASE (fmls, ls_expr, lcase)
        end
      else if str_eq ss "NULLLAMBDACASE"
      then NULLLAMBDACASE
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse lambda_case 1")
        end
    end
    | _ =>
      let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse lambda_case 1")
      end

and parse_datum (v : S.value) : datum =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    if str_eq (A.toString s) "INTLIT"
    then
      case L.nth (rst, 0) of
        S.INT i => INTLIT (IntInf.toInt i)
      | _ =>
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse datum")
        end
    else
      let
        val _ = Pr.print (TextIO.stdOut, v)
      in
        raise Fail ("cannot parse datum")
      end

  | _ => raise Fail "cannot raise datum"

and parse_formals (v : S.value) : formals =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "F1"
      then F1 (parse_list_sym (L.nth (rst, 0)))
      else if str_eq ss "F2"
      then
        let
          val ls_sym = parse_list_sym (L.nth (rst, 0))
          val sss = parse_sym (L.nth (rst, 1))
        in
          F2 (ls_sym, sss)
        end
      else if str_eq ss "F3"
      then F3 (parse_sym (L.nth (rst, 0)))
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse formals")
        end
    end
  | _ =>
    let
      val _ = Pr.print (TextIO.stdOut, v)
    in
      raise Fail ("cannot parse formals")
    end

and parse_list_toplvl (v : S.value) : list_toplvl =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "CONSTOPLVL"
      then
        let
          val tplvl = parse_toplvl (L.nth (rst, 0))
          val ls_toplvl = parse_list_toplvl (L.nth (rst, 1))
        in
          CONSTOPLVL (tplvl, ls_toplvl)
        end
      else if str_eq ss "NULLTOPLVL"
      then NULLTOPLVL
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse toplvl")
        end
    end
  | _ =>
      let
        val _ = Pr.print (TextIO.stdOut, v)
      in
        raise Fail ("cannot parse list_toplvl")
      end


and parse_list_expr (v : S.value) : list_expr =
    case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "CONSEXPR"
      then
        let
          val exp = parse_expr (L.nth (rst, 0))
          val ls_expr = parse_list_expr (L.nth (rst, 1))
        in
          CONSEXPR (exp, ls_expr)
        end
      else if str_eq ss "NULLEXPR"
      then NULLEXPR
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse list_expr")
        end
    end
    | _ =>
      let
        val _ = Pr.print (TextIO.stdOut, v)
      in
        raise Fail ("cannot parse list_expr")
      end

and parse_list_sym (v : S.value) : list_sym =
  case v of
    S.LIST ((S.SYMBOL s) :: rst) =>
    let
      val ss = A.toString s
    in
      if str_eq ss "CONSSYM"
      then
        let
          val sss = parse_sym (L.nth (rst, 0))
          val ls_sym = parse_list_sym (L.nth (rst, 1))
        in
          CONSSYM (sss, ls_sym)
        end
      else if str_eq ss "NULLSYM"
      then NULLSYM
      else
        let
          val _ = Pr.print (TextIO.stdOut, v)
        in
          raise Fail ("cannot parse list_sym")
        end
    end
    | _ =>
      let
        val _ = Pr.print (TextIO.stdOut, v)
      in
        raise Fail ("cannot parse list_sym")
      end
