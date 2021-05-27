(* Course compiler *)

structure L = List
structure LP = ListPair
structure P = MLton.Parallel

(* --------------------------------------------------------------------------------
 * -- Data types common to all languages *)

type sym = int
type ty = sym
type label = sym
type var = sym

fun symToString s = Int.toString s

datatype arg = IntArg of int | TrueArg | FalseArg | VarArg of var

fun print_arg arg =
  case arg of
    IntArg (i) => print ("(IntArg " ^ Int.toString i ^ ")")
  | TrueArg => print "(TrueArg)"
  | FalseArg => print "(FalseArg)"
  | VarArg (v) => print ("(VarArg " ^ symToString v ^ ")")

datatype prim = AddP | SubP | AndP | OrP

fun print_prim p =
  case p of
    AddP => print "AddP"
  | SubP => print "SubP"
  | AndP => print "AndP"
  | OrP => print "OrP"

datatype cmp = EqP | LtP

fun print_cmp p =
  case p of
    EqP => print "EqP"
  | LtP => print "LtP"

(* -------------------------------------------------------------------------------- *)
(*  * -- Environments *)

type typeEnv = (sym * sym) list
type varEnv = (sym * sym) list
type aliasEnv = (sym * sym) list

val default_int = 0
val default_sym = 0

fun lookup_with_default (def : 'a) (k : var) (env : (sym * 'a) list) =
  case (L.find (fn (k1,v) => k = k1) env) of
    SOME (_, v) => v
  | NONE => def

val empty_env = []

fun insert_env env k v = (k, v) :: env

fun lookup_env env k = lookup_with_default default_sym k env

fun contains_env env k =
    let
      val v = lookup_with_default default_sym k env
    in
      if v = default_sym
      then false
      else true
    end

type homesEnv = (sym * int) list

val empty_int_env = []

fun insert_int_env env k v = (k, v) :: env

fun lookup_int_env env k = lookup_with_default default_int k env

fun contains_int_env env k =
    let
      val v = lookup_with_default default_int k env
    in
      if v = default_int
      then false
      else true
    end

(* -------------------------------------------------------------------------------- *)


val intTy = 0
val boolTy = 1
val errorTy = 1

(* -------------------------------------------------------------------------------- *)
(*  * -- ANF'd Source *)

datatype simplExpA = ArgA of arg
                   | ReadA
                   | NegA of arg
                   | NotA of arg
                   | PrimA of (prim * arg * arg)
                   | CmpA of (cmp * arg * arg)

datatype expA = SimplA of simplExpA
              | LetA of (var * simplExpA *expA)
              | LetA2 of (var * simplExpA * expA)
              | IfA of (simplExpA * expA * expA)

datatype lang_a = ProgramA of (ty * expA)
                | ErrorA of ty

fun print_simpl_expa exp =
  case exp of
    ArgA (a) => (print ("(ArgA ") ; print_arg a ; print (")"))
  | ReadA    => print ("(ReadA)")
  | NegA (a) => (print ("(NegA ") ; print_arg a ; print (")"))
  | NotA (a) => (print ("(NotA ") ; print_arg a ; print (")"))
  | PrimA (p,a1,a2) => (print ("(PrimA " ) ;
                       print_prim p ;
                       print (" ") ;
                       print_arg a1 ;
                       print (" ") ;
                       print_arg a2 ;
                       print (")"))
  | CmpA (p,a1,a2) => (print ("(CmpA " ) ;
                      print_cmp p ;
                      print (" ") ;
                      print_arg a1 ;
                      print (" ") ;
                      print_arg a2 ;
                      print (")"))

fun print_expa exp =
  case exp of
    SimplA (simpl) => (print ("(SimplA ") ; print_simpl_expa simpl ; print (")"))
  | LetA (v, rhs, bod) => (print ("(LetA " ^ symToString v ^ " ") ;
                          print_simpl_expa rhs ;
                          print_expa bod ;
                          print (")"))
  | LetA2 (v, rhs, bod) => (print ("(LetA2 " ^ symToString v ^ " ") ;
                           print_simpl_expa rhs ;
                           print_expa bod ;
                           print (")"))
  | IfA (a, b, c) => (print ("(IfA ") ;
                     print_simpl_expa a ;
                     print (" ") ;
                     print_expa b ;
                     print (" ") ;
                     print_expa c ;
                     print (")"))

fun print_program_a p =
  case p of
    ProgramA (ty, exp) => (print ("(ProgramA " ^ symToString ty ^ " ") ;
                          print_expa exp ;
                          print (")"))

  | ErrorA (err) => print ("(ErrorA " ^ symToString err ^ ")")


(* -------------------------------------------------------------------------------- *)
(*  * -- C *)

datatype expC = ArgC of arg
              | ReadC
              | NegC of arg
              | NotC of arg
              | PrimC of (prim * arg * arg)
              | CmpC of (cmp * arg * arg)

datatype stmC = AssignC of (var * expC)

datatype tailC = RetC of expC
               | SeqC of (stmC * tailC)
               | IfC of (label * label * expC)
               | GotoC of label

datatype blkC = BlockCons of (label * tailC * blkC)
              | BlockNil
              | BlockAppend of (blkC * blkC)

datatype tailAndBlk = MkTailAndBlk of (tailC * blkC)

datatype lang_c = ProgramC of (ty * (sym list) * blkC)
                | ErrorC of ty

fun print_expc exp =
  case exp of
    ArgC (a) => (print ("(ArgC ") ; print_arg a ; print (")"))
  | ReadC    => (print ("(ReadC)"))
  | NegC (a) => (print ("(NegC ") ; print_arg a ; print (")"))
  | NotC (a) => (print ("(NotC ") ; print_arg a ; print (")"))
  | PrimC (p,a1,a2) => (print ("(PrimC " ) ;
                       print_prim p ;
                       print (" ") ;
                       print_arg a1 ;
                       print (" ") ;
                       print_arg a2 ;
                       print (")"))
  | CmpC (p,a1,a2) => (print ("(CmpC " ) ;
                      print_cmp p ;
                      print (" ") ;
                      print_arg a1 ;
                      print (" ") ;
                      print_arg a2 ;
                      print (")"))


fun print_stm (AssignC (v, exp)) = (print ("(AssignC " ^ symToString v ^ " ") ;
                                   print_expc exp ;
                                   print (")"))

fun print_tail tail =
  case tail of
    RetC (exp) => (print ("(RetC ") ; print_expc exp ; print (")"))
  | SeqC (stm, tail) => (print ("(SeqC ") ;
                        print_stm stm ;
                        print (" ") ;
                        print_tail tail ;
                        print (")"))

  | IfC (thn, els, cmp)  => (print ("(IfC " ^ symToString thn ^ " " ^ symToString els) ;
                            print_expc cmp ;
                            print (")"))

  | GotoC lbl => print ("(GotoC " ^ symToString lbl ^ ")")

fun print_blk blk =
  case blk of
    BlockCons (lbl, tail, rst) => (print ("(BlockCons " ^ symToString lbl ^ " ") ;
                                  print_tail tail ;
                                  print (" ") ;
                                  print_blk rst ;
                                  print (")"))
  | BlockNil => print ("(BlockNil)")
  | BlockAppend (b1, b2) => (print ("(BlockAppend " ) ;
                            print_blk b1 ;
                            print (" ") ;
                            print_blk b2 ;
                            print (")"))

fun print_locals ls =
  case ls of
    [] => ()
  | (x :: xs) => (print (symToString x ^ " ") ;
                  print_locals xs)

fun print_program_c p =
  case p of
    ProgramC (ty, locals, blk) => (print ("(ProgramC " ^ symToString ty ^ " (") ;
                                  print_locals locals ;
                                  print (")") ;
                                  print_blk blk ;
                                  print (")"))
  | ErrorC (err) => print ("(ErrorC " ^ symToString err ^ ")")

(* -------------------------------------------------------------------------------- *)
(*  * -- X86 with variables *)

type reg = string

fun regToString r = r

datatype argX86 = IntX86 of int
                | VarX86 of var
                | RegX86 of reg
                | DerefX86 of (reg * int)


datatype instrs = InstrCons of (instr * instrs)
                | InstrNil
                | InstrAppend of (instrs * instrs)

and instr = AddQ of (argX86 * argX86)
          | SubQ of (argX86 * argX86)
          | NegQ of argX86
          | XorQ of (argX86 * argX86)
          | SetEQ of reg
          | CmpQ of (argX86 * argX86)
          | MovQ of (argX86 * argX86)
          | MovzbQ of (argX86 * argX86)
          | JumpQ of label
          | JumpEQ of label
          | PushQ of argX86
          | PopQ of argX86
          | RetQ

datatype pseudoX86 = ProgramX86 of (ty * (sym list) * instrs)
                   | ErrorX86 of ty

fun print_reg r = print ("% " ^ regToString r)

fun print_argx86 a =
  case a of
    IntX86 (i) => print (Int.toString i)
  | VarX86 (v) => print (symToString v)
  | RegX86 (r) => print_reg r
  | DerefX86 (r, off) => (print (Int.toString off) ;
                          print ("(") ;
                          print_reg r ;
                          print (")"))

fun print_instrs instrs =
  case instrs of
    InstrCons (i, rst) => (print_instr i ;
                          print ("\n") ;
                          print_instrs rst)
  | InstrNil => ()
  | InstrAppend (xs, ys) =>  (print_instrs xs ;
                             print ("\n") ;
                             print_instrs ys)

and print_instr instr =
  case instr of
    AddQ (a1, a2) => (print ("addq ") ;
                     print_argx86 a1 ;
                     print ", " ;
                     print_argx86 a2)
  | SubQ (a1, a2) => (print ("subq ") ;
                     print_argx86 a1 ;
                     print ", " ;
                     print_argx86 a2)
  | NegQ (a1) => (print ("negq ") ; print_argx86 a1)
  | XorQ (a1, a2) => (print ("xorq ") ;
                     print_argx86 a1 ;
                     print ", " ;
                     print_argx86 a2)
  | SetEQ (r) => (print ("sete ") ; print_reg r)
  | CmpQ (a1, a2) => (print ("cmpq ") ;
                     print_argx86 a1 ;
                     print ", " ;
                     print_argx86 a2)
  | MovQ (a1, a2) => (print ("movq ") ;
                     print_argx86 a1 ;
                     print ", " ;
                     print_argx86 a2)
  | MovzbQ (a1, a2) => (print ("movzbq ") ;
                       print_argx86 a1 ;
                       print ", " ;
                       print_argx86 a2)
  | JumpQ (lbl) => print ("jmp " ^ symToString lbl)
  | JumpEQ (lbl) => print ("je " ^ symToString lbl)
  | PushQ (a1) => (print ("pushq ") ; print_argx86 a1)
  | PopQ (a1) => (print ("popq ") ; print_argx86 a1)
  | RetQ => print "retq"


fun print_pseudox86 p =
  case p of
    ProgramX86 (ty, locals, instrs) => (print ("(locals ") ;
                                       print_locals locals ;
                                       print (")\n") ;
                                       print_instrs instrs)

  | ErrorX86 (err) => print ("(ErrorX86 " ^ symToString err ^ ")")


(* -------------------------------------------------------------------------------- *)
(*  * -- Gensym *)

val global_gensym_counter = ref 0

fun gensym () =
    let
      val next = P.fetchAndAdd global_gensym_counter 1
    (* in "gensym_" ^ Int.toString next *)
    in next
    end

(* -------------------------------------------------------------------------------- *)
(*  * -- Uniqify *)

fun uniqify_arg var_env arg =
  case arg of
    VarArg (v) => VarArg (lookup_env var_env v)
  | _ => arg

fun uniqify_simpl_expa var_env exp =
  case exp of
    ArgA (a) => ArgA (uniqify_arg var_env a)
  | ReadA => ReadA
  | NegA (e) => NegA (uniqify_arg var_env e)
  | NotA (e) => NotA (uniqify_arg var_env e)
  | PrimA (p, e1, e2) => PrimA (p, (uniqify_arg var_env e1), (uniqify_arg var_env e2))
  | CmpA (c, e1, e2)  => CmpA (c, (uniqify_arg var_env e1), (uniqify_arg var_env e2))

fun uniqify_expa var_env exp =
  case exp of
    SimplA (simpl) => SimplA (uniqify_simpl_expa var_env simpl)
  | LetA (v, rhs, bod) =>
    (*
     if contains_env var_env v
     then
     *)
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val v' = gensym()
         val var_env' = insert_env var_env v v'
         val bod' = uniqify_expa var_env' bod
       in LetA (v', rhs', bod')
       end
    (*
     else
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val var_env' = insert_env var_env v v
         val bod' = uniqify_expa var_env' bod
       in LetA (v, rhs', bod')
       end
     *)
  | LetA2 (v, rhs, bod) =>
    (*
     if contains_env var_env v
     then
     *)
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val v' = gensym()
         val var_env' = insert_env var_env v v'
         val bod' = uniqify_expa var_env' bod
       in LetA2 (v', rhs', bod')
       end
     (*
     else
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val var_env' = insert_env var_env v v
         val bod' = uniqify_expa var_env' bod
       in LetA2 (v, rhs', bod')
       end
       *)
  | IfA (a,b,c) => IfA (uniqify_simpl_expa var_env a, uniqify_expa var_env b, uniqify_expa var_env c)

fun par3 (a, b, c, d) =
    let
      val ((ar, br), cr) =
        ForkJoin.par (fn _ => ForkJoin.par (a, b),
                      fn _ => c)
    in
      (ar, br, cr)
    end

fun uniqify_expa_par var_env exp =
  case exp of
    SimplA (simpl) => SimplA (uniqify_simpl_expa var_env simpl)
  | LetA (v, rhs, bod) =>
    (*
     if contains_env var_env v
     then
     *)
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val v' = gensym()
         val var_env' = insert_env var_env v v'
         val bod' = uniqify_expa_par var_env' bod
       in LetA (v', rhs', bod')
       end
    (*
     else
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val var_env' = insert_env var_env v v
         val bod' = uniqify_expa_par var_env' bod
       in LetA (v, rhs', bod')
       end
      *)
  | LetA2 (v, rhs, bod) =>
    (*
     if contains_env var_env v
     then
    *)
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val v' = gensym()
         val var_env' = insert_env var_env v v'
         val bod' = uniqify_expa var_env' bod
       in LetA2 (v', rhs', bod')
       end
    (*
     else
       let
         val rhs' = uniqify_simpl_expa var_env rhs
         val var_env' = insert_env var_env v v
         val bod' = uniqify_expa var_env' bod
       in LetA2 (v, rhs', bod')
       end
     *)
  | IfA (a,b,c) =>
    let
      val d = uniqify_simpl_expa var_env a
      val (e,f) = ForkJoin.par (fn _ => uniqify_expa_par var_env b,
                                fn _ => uniqify_expa_par var_env c)
    in IfA (d,e,f)
    end


fun uniqify prg =
  case prg of
    ProgramA (ty, exp) => ProgramA (ty, uniqify_expa empty_env exp)
  | ErrorA (err) => ErrorA (err)

fun uniqify_par prg =
  case prg of
    ProgramA (ty, exp) => ProgramA (ty, uniqify_expa_par empty_env exp)
  | ErrorA (err) => ErrorA (err)

(* -------------------------------------------------------------------------------- *)
(*  * -- Typecheck *)

fun typecheck_arg ty_env arg =
  case arg of
    IntArg(i) => intTy
  | TrueArg => boolTy
  | FalseArg => boolTy
  | VarArg (v) =>
     if contains_env ty_env v
     then lookup_env ty_env v
     else errorTy

fun typecheck_prim p t1 t2 =
  case p of
    AddP => if t1 = intTy andalso t2 = intTy
            then intTy
            else errorTy
  | SubP => if t1 = intTy andalso t2 = intTy
            then intTy
            else errorTy
  | AndP => if t1 = boolTy andalso t2 = boolTy
            then boolTy
            else errorTy
  | OrP => if t1 = boolTy andalso t2 = boolTy
           then boolTy
           else errorTy

fun typecheck_cmp p t1 t2 =
  case p of
    LtP => if t1 = intTy andalso t2 = intTy
           then boolTy
           else errorTy
  | EqP => if t1 = intTy andalso t2 = intTy
           then boolTy
           else errorTy

fun typecheck_simpl_expa ty_env exp =
  case exp of
    ArgA (arg) => typecheck_arg ty_env arg
  | ReadA => intTy
  | NegA (exp1) =>
    let
      val texp1 = typecheck_arg ty_env exp1
    in
      if texp1 = intTy
      then intTy
      else errorTy
    end
  | NotA (exp1) =>
    let
      val texp1 = typecheck_arg ty_env exp1
    in
      if texp1 = boolTy
      then intTy
      else errorTy
    end
  | PrimA (p, a1, a2) =>
    let
      val t1 = typecheck_arg ty_env a1
      val t2 = typecheck_arg ty_env a2
    in
      typecheck_prim p t1 t2
    end
  | CmpA (p, a1, a2) =>
     let
      val t1 = typecheck_arg ty_env a1
      val t2 = typecheck_arg ty_env a2
    in
      typecheck_cmp p t1 t2
     end

fun typecheck_expa ty_env exp =
  case exp of
    SimplA (simpl) => typecheck_simpl_expa ty_env simpl
  | LetA (v, rhs, bod) =>
     let
       val ty = typecheck_simpl_expa ty_env rhs
       val ty_env' = insert_env ty_env v ty
     in typecheck_expa ty_env' bod
     end
  | LetA2 (v, rhs, bod) =>
    let
      val ty = typecheck_simpl_expa ty_env rhs
      val ty_env' = insert_env ty_env v ty
    in typecheck_expa ty_env' bod
    end
  | IfA (a, b, c) =>
     let
       val t1 = typecheck_simpl_expa ty_env a
       val t2 = typecheck_expa ty_env b
       val t3 = typecheck_expa ty_env c
     in
       if t1 = boolTy
       then if t2 = t3
            then t2
            else errorTy
       else errorTy
     end

fun typecheck_expa_par ty_env exp =
  case exp of
    SimplA (simpl) => typecheck_simpl_expa ty_env simpl
  | LetA (v, rhs, bod) =>
     let
       val ty = typecheck_simpl_expa ty_env rhs
       val ty_env' = insert_env ty_env v ty
     in typecheck_expa_par ty_env' bod
     end
  | LetA2 (v, rhs, bod) =>
    let
      val ty = typecheck_simpl_expa ty_env rhs
      val ty_env' = insert_env ty_env v ty
    in typecheck_expa ty_env' bod
    end
  | IfA (a, b, c) =>
     let
       val t1 = typecheck_simpl_expa ty_env a
       val (t2, t3) = ForkJoin.par (fn _ => typecheck_expa_par ty_env b,
                                    fn _ => typecheck_expa_par ty_env c)
     in
       if t1 = boolTy
       then if t2 = t3
            then t2
            else errorTy
       else errorTy
     end


fun typecheck prg =
  case prg of
    ProgramA (expected, exp) =>
    let
      val actual = typecheck_expa empty_env exp
    in
      if expected = actual
      then ProgramA (expected, exp)
      else ErrorA errorTy
    end
  | ErrorA (err) => ErrorA (err)

fun typecheck_par prg =
  case prg of
    ProgramA (expected, exp) =>
    let
      val actual = typecheck_expa_par empty_env exp
    in
      if expected = actual
      then ProgramA (expected, exp)
      else ErrorA errorTy
    end
  | ErrorA (err) => ErrorA (err)

(* -------------------------------------------------------------------------------- *)
(*  * -- Explicate control *)

fun to_expc exp =
  case exp of
    ArgA (a) => ArgC (a)
  | ReadA => ReadC
  | NegA (a) => NegC (a)
  | NotA (a) => NotC (a)
  | PrimA (p, a1, a2) => PrimC (p, a1, a2)
  | CmpA (p, a1, a2) => CmpC (p, a1, a2)

fun explicate_tail2 exp =
  case exp of
    SimplA (simpl) => ([], (RetC (to_expc simpl)))
  | LetA2 (v, rhs, bod) =>
    let
      val rhs' = to_expc rhs
      val stm = AssignC (v, rhs')
      val (locals, tail) = explicate_tail2 bod
      val locals' = v :: locals
      val tail' = SeqC (stm, tail)
    in (locals', tail')
    end
  | _ => raise (Fail "explicate_tail2")

fun explicate_tail exp =
  case exp of
    SimplA (simpl) =>
    let
      val tb = MkTailAndBlk (RetC (to_expc simpl), BlockNil)
    in
      ([], tb)
    end
  | LetA2 (v, rhs, bod) =>
     let
       val (locals, tail) = explicate_tail2 exp
       val tb = MkTailAndBlk (tail, BlockNil)
     in (locals, tb)
     end
  | LetA (v, rhs, bod) =>
    let
      val rhs' = to_expc rhs
      val (locals, (MkTailAndBlk (tl, blk))) = explicate_tail bod
      val stm = AssignC (v, rhs')
      val tail = SeqC (stm, tl)
      val locals' = v :: locals
    in (locals', MkTailAndBlk (tail, blk))
    end
  | IfA (a,b,c) =>
    let
      val a' = to_expc a
      val (locals1, MkTailAndBlk(thn_tail, thn_blocks)) = explicate_tail b
      val (locals2, MkTailAndBlk(els_tail, els_blocks)) = explicate_tail c
      val locals3 = L.revAppend (locals1, locals2)
      val thn_label = gensym()
      val els_label = gensym()
      val tail' = IfC (thn_label, els_label, a')
      val blks0 = BlockCons (thn_label, thn_tail, thn_blocks)
      val blks1 = BlockCons (els_label, els_tail, els_blocks)
      val blks2 = BlockAppend (blks0, blks1)
      val tb = MkTailAndBlk (tail', blks2)
    in
      (locals3, tb)
    end


fun explicate_tail_par exp =
  case exp of
    SimplA (simpl) =>
    let
      val tb = MkTailAndBlk (RetC (to_expc simpl), BlockNil)
    in
      ([], tb)
    end
  | LetA2 (v, rhs, bod) =>
     let
       val (locals, tail) = explicate_tail2 exp
       val tb = MkTailAndBlk (tail, BlockNil)
     in (locals, tb)
     end
  | LetA (v, rhs, bod) =>
    let
      val rhs' = to_expc rhs
      val (locals, (MkTailAndBlk (tl, blk))) = explicate_tail_par bod
      val stm = AssignC (v, rhs')
      val tail = SeqC (stm, tl)
      val locals' = v :: locals
    in (locals', MkTailAndBlk (tail, blk))
    end
  | IfA (a,b,c) =>
    let
      val a' = to_expc a
      val (t1, t2) = ForkJoin.par (fn _ => explicate_tail_par b,
                                   fn _ => explicate_tail_par c)
      val (locals1, MkTailAndBlk(thn_tail, thn_blocks)) = t1
      val (locals2, MkTailAndBlk(els_tail, els_blocks)) = t2
      val locals3 = L.revAppend (locals1, locals2)
      val thn_label = gensym()
      val els_label = gensym()
      val tail' = IfC (thn_label, els_label, a')
      val blks0 = BlockCons (thn_label, thn_tail, thn_blocks)
      val blks1 = BlockCons (els_label, els_tail, els_blocks)
      val blks2 = BlockAppend (blks0, blks1)
      val tb = MkTailAndBlk (tail', blks2)
    in
      (locals3, tb)
    end


fun explicate_control prg =
  case prg of
    ProgramA (ty, exp) =>
      let
        val (locals, (MkTailAndBlk (tail, blk0))) = explicate_tail exp
        val start = gensym()
        val blk2 = BlockCons (start, tail, blk0)
      in ProgramC (ty, locals, blk2)
      end
  | ErrorA (err) => ErrorC (err)

fun explicate_control_par prg =
  case prg of
    ProgramA (ty, exp) =>
      let
        val (locals, (MkTailAndBlk (tail, blk0))) = explicate_tail_par exp
        val start = gensym()
        val blk2 = BlockCons (start, tail, blk0)
      in ProgramC (ty, locals, blk2)
      end
  | ErrorA (err) => ErrorC (err)


(* -------------------------------------------------------------------------------- *)
(*  * -- Select instructions *)

fun select_instrs_arg arg =
  case arg of
    IntArg (i) => IntX86 i
  | TrueArg => IntX86 1
  | FalseArg => IntX86 0
  | VarArg v => VarX86 v

fun select_instrs_exp target exp rst =
  case exp of
    ArgC arg =>
    let
      val arg' = select_instrs_arg arg
    in
      InstrCons (MovQ (arg', target), rst)
    end
  | ReadC => raise (Fail "select_instrs_exp: ReadC")
  | NegC arg =>
    let
      val arg' = select_instrs_arg arg
    in
      InstrCons (MovQ (arg', target), InstrCons (NegQ target, rst))
    end
  | NotC arg =>
    let
      val arg' = select_instrs_arg arg
    in InstrCons (MovQ (arg', target), InstrCons (XorQ (IntX86 1, target), rst))
    end
  | PrimC (p, a1, a2) =>
    let
      val a1' = select_instrs_arg a1
      val a2' = select_instrs_arg a2
      val f = (fn x => case p of AddP => AddQ x
                               | SubP => SubQ x
                               | AndP => raise (Fail "select_instrs_exp: AndP")
                               | OrP => raise (Fail "select_instrs_exp: OrP"))

      val instr1 = MovQ (a1', target)
      val instr2 = f (a2', target)
    in
      InstrCons (instr1, InstrCons (instr2, rst))
    end
  | CmpC (c, a1, a2) =>
    let
      val a1' = select_instrs_arg a1
      val a2' = select_instrs_arg a2
      val instr1 = CmpQ (a1', a2')
      val instr2 = case c of EqP => SetEQ ("al")
                           | LtP => SetEQ ("al")
      val instr3 = MovzbQ (RegX86 "al", target)

    in InstrCons (instr1, InstrCons (instr2, InstrCons (instr3, rst)))
    end

fun select_instrs_tail tail =
  case tail of
    RetC exp =>
    let
      val target = RegX86 "rax"
    in
      select_instrs_exp target exp InstrNil
    end
  | SeqC (AssignC (v, rhs), rst) =>
     let
       val target = VarX86 v
       val instrs_rst = select_instrs_tail rst
     in
       select_instrs_exp target rhs instrs_rst
     end
  | IfC (thn, els, cmp) =>
    let
      val instrs_rst = InstrCons (JumpEQ thn, InstrCons (JumpQ els, InstrNil))
      val target = RegX86 "rbx"
    in select_instrs_exp target cmp instrs_rst
    end
  | GotoC lbl => raise (Fail "select_instrs_tail: GotoC")

fun select_instrs_blk blk =
  case blk of
    BlockNil => InstrNil
  | BlockCons (lbl, tail, rst) =>
     let
       val instrs1 = select_instrs_tail tail
       val instrs2 = select_instrs_blk rst
     in InstrAppend (instrs1, instrs2)
     end
  | BlockAppend (blk1, blk2) =>
    let
      val instrs1 = select_instrs_blk blk1
      val instrs2 = select_instrs_blk blk2
    in InstrAppend (instrs1, instrs2)
    end

fun select_instrs_blk_par blk =
  case blk of
    BlockNil => InstrNil
  | BlockCons (lbl, tail, rst) =>
     let
       val (instrs1, instrs2) = ForkJoin.par (fn _ => select_instrs_tail tail,
                                              fn _ => select_instrs_blk rst)
       (* val (instrs1, instrs2) = (select_instrs_tail tail, *)
       (*                           select_instrs_blk_par rst) *)
     in InstrAppend (instrs1, instrs2)
     end
  | BlockAppend (blk1, blk2) =>
    let
      val (instrs1, instrs2) = ForkJoin.par (fn _ => select_instrs_blk_par blk1,
                                             fn _ => select_instrs_blk_par blk2)
    in InstrAppend (instrs1, instrs2)
    end


fun select_instrs prg =
  case prg of
    ProgramC (ty, locals, blk) => ProgramX86 (ty, locals, select_instrs_blk blk)
  | ErrorC (err) => ErrorX86 (err)

fun select_instrs_par prg =
  case prg of
    ProgramC (ty, locals, blk) => ProgramX86 (ty, locals, select_instrs_blk_par blk)
  | ErrorC (err) => ErrorX86 (err)

(* -------------------------------------------------------------------------------- *)
(*  * -- Assign homes *)

fun make_homes ls : homesEnv =
    let
      val em = empty_int_env
      val indices = L.tabulate (L.length ls, (fn i => i))
    in LP.foldl (fn ((v : var), (i : int), acc) =>
                     let val stack_loc = 0 - (8 + (8 * i))
                     in insert_int_env acc v stack_loc
                     end)
                em (ls, indices)
    end

fun assign_homes_arg homes arg =
  case arg of
    VarX86 v =>
     (* let o = lookup_int_env homes v in *)
    let
      val off = lookup_int_env homes v
      (* val off = 1 *)
    in
      DerefX86 ("rbp", off)
    end
  | _ => arg

fun assign_homes_instr homes instr =
  case instr of
    AddQ (a1, a2) => AddQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | SubQ (a1, a2) => SubQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | NegQ (a1) => NegQ (assign_homes_arg homes a1)
  | XorQ (a1, a2) => XorQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | SetEQ (r) => SetEQ (r)
  | CmpQ (a1, a2) => CmpQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | MovQ (a1, a2) => MovQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | MovzbQ (a1, a2) => MovzbQ (assign_homes_arg homes a1, assign_homes_arg homes a2)
  | JumpQ (lbl) => JumpQ (lbl)
  | JumpEQ (lbl) => JumpEQ (lbl)
  | PushQ (a1) => PushQ (assign_homes_arg homes a1)
  | PopQ (a1) => PopQ (assign_homes_arg homes a1)
  | RetQ => RetQ

fun assign_homes_instrs homes instrs =
  case instrs of
    InstrCons (instr, rst) =>
      let
        val instr' = assign_homes_instr homes instr
        val rst' = assign_homes_instrs homes rst
      in InstrCons (instr', rst')
      end
  | InstrNil => InstrNil
  | InstrAppend (instrs1, instrs2) =>
    let
      val instrs1' = assign_homes_instrs homes instrs1
      val instrs2' = assign_homes_instrs homes instrs2
    in InstrAppend (instrs1', instrs2')
    end

fun assign_homes_instrs_par homes instrs =
  case instrs of
    InstrCons (instr, rst) =>
      let
        val instr' = assign_homes_instr homes instr
        val rst' = assign_homes_instrs homes rst
      in InstrCons (instr', rst')
      end
  | InstrNil => InstrNil
  | InstrAppend (instrs1, instrs2) =>
    let
      val (instrs1', instrs2') = ForkJoin.par (fn _ => assign_homes_instrs_par homes instrs1,
                                               fn _ => assign_homes_instrs_par homes instrs2)
    in InstrAppend (instrs1', instrs2')
    end


fun assign_homes prg =
  case prg of
    ProgramX86 (ty, locals, instrs) =>
    let
      val homes = make_homes locals
      (* val homes = empty_env *)
    in
      ProgramX86 (ty, [], assign_homes_instrs homes instrs)
    end
  | ErrorX86 err => ErrorX86 err

fun assign_homes_par prg =
  case prg of
    ProgramX86 (ty, locals, instrs) =>
    let
      val homes = make_homes locals
      (* val homes = empty_env *)
    in
      ProgramX86 (ty, [], assign_homes_instrs_par homes instrs)
    end
  | ErrorX86 err => ErrorX86 err

(* -------------------------------------------------------------------------------- *)

fun compile p0 =
    let
      (* val _ = print_program_a p0 *)
      val p1 = typecheck p0
      (* val _ = print_program_a p1 *)
      val p2 = uniqify p1
      (* val _ = print_program_a p2 *)
      val p3 = explicate_control p2
      val p4 = select_instrs p3
      val p5 = assign_homes p4
    in p5
    end

fun compile_par p0 =
    let
      val p1 = typecheck_par p0
      val p2 = uniqify_par p1
      (* val _ = print_program_a p2 *)
      val p3 = explicate_control_par p2
      val p4 = select_instrs_par p3
      val p5 = assign_homes_par p4
    in p5
    end


fun make_big_ex2 n =
  if n <= 0
  then SimplA (ArgA (IntArg 1))
  else
    (* let v2 = gensym *)
    let
      val v2 = 1
    in
      LetA2 (v2, (ArgA (IntArg (n-1))), make_big_ex2 (n-1))
    end

fun make_big_ex n d =
  (* (* SMALL *) *)
  (* if d > 6 *)
  (* OTHERWISE *)
  if d > 10
  then make_big_ex2 n
  else
    let
      val v1 = 0
      val branch = make_big_ex n (d+1)
    in
      (* LetA (v1, ArgA (IntArg n), ) *)
      IfA (CmpA (EqP, (IntArg n), (IntArg 0)), branch, branch)
    end
