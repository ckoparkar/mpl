datatype exp =
         Lit of int
       | MkTrue
       | MkFalse
       | Plus of (exp * exp)
       | And of (exp * exp)
       | Or of (exp * exp)

fun fold_constants exp =
    case exp of
        Lit i => Lit i
      | MkTrue => MkTrue
      | MkFalse => MkFalse
      | Plus (Lit i, Lit j) => Lit (i+j)
      | Plus (a, b) =>
        Plus (fold_constants a, fold_constants b)

fun fold_constants_par depth exp =
    if depth > 6 then fold_constants exp else
    case exp of
        Lit i => Lit i
      | MkTrue => MkTrue
      | MkFalse => MkFalse
      | Plus (Lit i, Lit j) => Lit (i+j)
      | Plus (a, b) =>
        let
          val (c,d) = ForkJoin.par ((fn _ => fold_constants_par (depth+1) a),
                                    (fn _ => fold_constants_par (depth+1) b))
        in Plus (c, d)
        end

fun sum_exp e =
    case e of
        Lit i => i
      | Plus (a, b) => (sum_exp a) + (sum_exp b)

fun build_exp n =
    if n = 0
    then Plus (Lit 0, Lit 1)
    else Plus (build_exp (n-1), build_exp (n-1))
