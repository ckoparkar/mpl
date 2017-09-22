structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

val takeLock = _import "Parallel_lockTake" runtime private: int ref -> unit;
val releaseLock = _import "Parallel_lockRelease" runtime private: int ref -> unit;

val printLock = ref ~1

val tasksAdded = ref 0
val tasksRun = ref 0

fun print s = ()
(*
    (takeLock printLock;
     TextIO.print s;
     releaseLock printLock)
*)

fun eprint s = ()
                   (*
    (takeLock printLock;
     TextIO.print s;
     releaseLock printLock)
*)

val debug = false


type void = unit
type work = unit -> void
type proc = int

exception WorkQueue
exception QueueSize

structure P = Priority
structure T = MLton.Thread
structure A = Array
structure V = Vector
structure Tm = Time

  datatype job = JWork of (unit -> void)
               | Thread of (unit T.t)
               | RThread of T.Runnable.t * int

(* type work = unit -> void *)

  val numberOfProcessors = MLtonParallelInternal.numberOfProcessors

  val processorNumber = MLtonParallelInternal.processorNumber
  val P = numberOfProcessors

val yield = _import "Parallel_yield" runtime private: unit -> unit;
fun compareAndSwap (r, old, new) =
    MLton.Parallel.compareAndSwap r (old, new) = old
fun fetchAndAdd (r, d) = MLton.Parallel.fetchAndAdd r d

structure S = MLton.Signal
val pblocksig = _import "Parallel_block_sig" runtime private: S.t -> unit;
val punblocksig = _import "Parallel_unblock_sig" runtime private: S.t -> unit;

val isblocked =_import "Parallel_check_blocked" runtime private: S.t -> bool;
(*
fun isblocked s = S.Mask.isMember (S.Mask.getBlocked (), s)
*)

val refToInt = _import "refToInt" runtime private: int ref -> int;

val prim_sig = MLton.Itimer.signal MLton.Itimer.Real
val sec_sig = Posix.Signal.usr1

val inCriticalSection = Array.array (numberOfProcessors, false)

fun assertblocked s =
    let val p = processorNumber ()
    in
        if Array.sub (inCriticalSection, p) then
            ()
        else
            eprint ("Signal not blocked at " ^ s ^ " " ^ (Int.toString p) ^ "\n")
    end
(*
    let val p = processorNumber ()
        val ib = isblocked (if p = 0 then prim_sig else sec_sig)
    in
        if ib then ()
        else eprint ("Signal not blocked at " ^ s ^ " " ^ (Int.toString p) ^ "\n")
    end
*)

val delta =
    case
        (case CommandLine.arguments () of
             [] => NONE
           | us::_ => Int.fromString us)
     of
        NONE => 5000 (* default in microseconds *)
      | SOME us => us
val delta2 = 200 (* microseconds *)



fun block_sigs p =
    ((*(if p = 0 then
          (* pblocksig prim_sig *)
          S.Mask.block (S.Mask.some [prim_sig])
        (* S.Mask.block S.Mask.all *)
    else
        (* pblocksig sec_sig *)
        S.Mask.block (S.Mask.some [sec_sig])
        (* S.Mask.block S.Mask.all *)); *)
     Array.update (inCriticalSection, p, true))

fun unblock_sigs p =
    (Array.update (inCriticalSection, p, false) (* ;
     if p = 0 then
         (* punblocksig prim_sig *)
         S.Mask.unblock (S.Mask.some [prim_sig])
     else
         (* punblocksig sec_sig) *)
         S.Mask.unblock (S.Mask.some [sec_sig]) *))

(*
fun atomically f =
    let val p = processorNumber () in
        block_sigs p;
        T.atomically f
        before unblock_sigs p
    end
*)

  datatype 'a t = Suspend of P.t * 'a T.t
                | Capture of P.t * 'a T.t

  val ioqueues = Array.array (numberOfProcessors, [])
  val workingOnPriority = Array.array (numberOfProcessors, P.bot)
  val lookingForWork = Array.array (numberOfProcessors, true)

  fun currentPrio () =
      let val p = processorNumber ()
      in
          A.sub (workingOnPriority, p)
      end

  val WORK_ARRAY_SIZE = 1024

(* private state *)
datatype token = Token of int * int * queue option ref
and entry = Empty | Work of (token * job (* * int *))
and queue = Queue of
            {
              top : int ref,
              bottom : int ref,
              work : entry A.array ref,
              prioind : P.t
            }

fun newQueue prio =
    Queue {
      top = ref 0,
      bottom = ref 0,
      work = ref (A.array (WORK_ARRAY_SIZE, Empty)),
      prioind = prio
    }

val queues = ref (A.fromList [])


val successfulSteals = ref 0
fun resetSteals () = successfulSteals := 0
fun incSteals () = successfulSteals := !successfulSteals + 1
fun reportSuccessfulSteals () = !successfulSteals
fun reportFailedSteals () = 0

val notwaiting = ~1
val notask = ~2
fun flag_of_prio (p: P.t) =
    ~2 - (P.toInt p)
fun prio_of_flag (f: int) =
    (~2 - f)

type cellval = (bool * job) option
val commcells = V.tabulate (P, fn _ => ref notwaiting)
val taskcells = A.array (P, NONE)
val nextdeal = A.array (P, Tm.zeroTime)
val reqcells = V.tabulate (P, fn _ => ref ~1)

val nextToken = A.array (P, 0)

fun newWork p =
    let val i = A.sub (nextToken, p)
    in
      Token (p, i, ref NONE)
      before A.update (nextToken, p, i + 1)
    end

fun stringOfToken (Token (p, i, _)) = "(" ^ (Int.toString p) ^ ", " ^
    (Int.toString i) ^ ")"

fun resize (Queue {top, bottom, work, ... }) =
    let
        val i = !top
        val j = !bottom
        val w = !work
        val l = A.length w
    in
        if i - j < l div 2 then
            let
                val () = print "copying!"
                fun erase k = if k = i then ()
                              else (A.update (w, k, Empty);
                                    erase (k + 1))
                fun copy k = if k = i then erase j
                             else (A.update (w, k - j, A.sub (w, k));
                                   copy (k + 1))
            in
                copy j;
                top := i - j;
                bottom := 0
            end
        else
            let
                val () = print "resizing!"
                val w = A.tabulate (l * 2,
                                    fn k => if k < (i - j) then A.sub (w, k + j)
                                            else Empty)
            in
                work := w;
                top := i - j;
                bottom := 0
            end
    end

fun queuesWithWork prio =
    let fun hasWork p =
            let val q = A.sub (A.sub (!queues, prio), p)
                val Queue {top, bottom, ...} = q
                val i = !top
                val j = !bottom
            in
                if i <> j then
                    ((* print ((Int.toString p) ^ " has " ^ (Int.toString (i - j)) ^ "\n"); *)
                     true)
                else false
            end
    in
        List.length (List.filter hasWork (List.tabulate (P, fn i => i)))
    end

fun add (q as Queue {top, bottom, work, ... }) (tw as (t, w)) =
    let
        val _ = assertblocked "add"
        val i = !top
        val () = if i = A.length (!work) then resize q else ()
        val i = !top (* in case of resize *)
        val Token (_, _, qr) = t
    in
        (A.update (!work, i, Work tw)
         (* handle e => (print "here 159\n"; raise e) *));
        qr := SOME q;
        top := i + 1;
        if debug then ignore (fetchAndAdd (tasksAdded, 1)) else ()
    end

fun popTop (q as Queue {top, bottom, work, ... }) =
    let val _ = assertblocked "popTop"
        val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then raise WorkQueue (* empty *)
        else
            (bottom := j + 1;
             (if debug then ignore (fetchAndAdd (tasksAdded, ~1)) else ());
             case A.sub (w, j) of
                 Empty => raise WorkQueue
               | Work (t, w) => w)
            (* handle e => (print "here 215\n"; raise e) *)
    end

fun topIsRThread (q as Queue {top, bottom, work, ... }) =
    let val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then false else
        case A.sub (w, j) of
            Work (_, RThread _) => true
          | _ => false
    end

fun popBottom  (q as Queue {top, bottom, work, ... }) =
    let val _ = assertblocked "popBottom"
        val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then raise WorkQueue (* empty *)
        else
            (top := i - 1;
             (if debug then ignore (fetchAndAdd (tasksAdded, ~1)) else ());
             case A.sub (w, i - 1) of
                 Empty => raise WorkQueue
               | Work (t, w) => w)
            (* handle e => (print "here 240\n"; raise e) *)
    end

val lastDealtTo = Array.array (P, ~1)

fun peekWorkPrio prioind p =
    let (* val _ = eprint ("Get work at " ^ (Int.toString prioind) ^ "\n") *)
        val q = A.sub (A.sub (!queues, prioind - 1), p)
        val Queue {top, bottom, work, ...} = q
        val i = !top
        val j = !bottom
    in
        i <> j
    end

fun dealAttempt p =
    let (* val _ = eprint "deal\n" *)
        val p' =
            (if !(V.sub (reqcells, p)) = ~1 then
                (p + 1 + LargeInt.toInt
                             (LargeInt.mod (Word.toLargeInt (MLton.Random.rand ()),
                                            LargeInt.fromInt (P - 1))))
                mod P
            else !(V.sub (reqcells, p)))
                (* handle e => (eprint "here 285\n"; raise e) *)
        (* val _ = print ((Int.toString P) ^ "\n")
        val _ = print ((Word.toString r) ^ "\n")
        val _ = print ((Int.toString (Word.toIntX r)) ^ "\n") *)
        (* val _ = print ((Int.toString p) ^ " attempting to deal to " ^
                       (Int.toString p') ^ "\n") *)
        val c = V.sub (commcells, p')
        val vv = !c
        val prios = P.count ()
        fun searchForAny i =
            if i <= 0 then NONE
            else
                if peekWorkPrio i p then SOME (i, P.fromInt i)
                else searchForAny (i - 1)
        fun searchForBetter i (pi, pp) =
            if i <= pi then NONE
            else
                let val prio' = P.fromInt i
                in
                    if P.plt(pp, prio') andalso peekWorkPrio i p
                    then SOME (i, prio')
                    else searchForBetter (i - 1) (pi, pp)
                end
        fun haveBetter pri =
            if pri = ~1 then NONE
            else if pri = ~2 then searchForAny prios
            else
                let val pi = prio_of_flag pri
                in
                    eprint ("haveBetter " ^ (Int.toString pi) ^ "\n");
                    searchForBetter prios (pi, P.fromInt pi)
                end
    in
        if p' = p orelse vv >= notwaiting
        then false
        else
            (case A.sub (taskcells, p) of
                SOME _ => (* have an outstanding deal *)
                (eprint ("Task hasn't been taken from " ^
                         (Int.toString p) ^ ": cell is " ^
                         (Int.toString (!(V.sub (commcells, A.sub (lastDealtTo, p))))) ^ "\n"); false)
              | NONE =>
                (case haveBetter vv of
                     NONE => false
                   | SOME (pi, pp) =>
                     if compareAndSwap (c, vv, p) then
                         let val q = A.sub (A.sub (!queues, pi - 1), p)
                         in
                             A.update (taskcells, p, SOME (pp, popTop q));
                             (* eprint ((Int.toString p) ^ " dealt a task to " ^ (Int.toString p') ^ "\n"); *)
                             A.update (lastDealtTo, p, p');
                             true
                         end
                     else
                         (* Someone else dealt a task *)
                         ((* print ((Int.toString p) ^ " failed at 281\n"); *) false)
                )
            )
    end

fun communicate p =
    if P > 1 andalso Time.> (Time.now (), A.sub (nextdeal, p)) then
        (((* print ("communicating on " ^ (Int.toString p) ^ "\n"); *)
         ((dealAttempt p)
          (* handle e => (eprint "here 219\n"; raise e) *));
         let val now = Time.now ()
             val mi = Math.pow (2.0, Real.fromInt (Word.wordSize - 1))
             val r = Word.mod (MLton.Random.rand (), Word.fromInt 1000000)
             val rnd = (Real./
                        (Real.fromInt ((Word.toInt r) + 1), 1000001.0))
                       (* handle e => (eprint "here 171\n"; raise e) *)
             val mult = (Math.ln rnd)
                       (* handle e => (eprint "here 353\n"; raise e) *)
             val us = (LargeInt.fromInt
                          (Real.floor (Real.* (Real.fromInt delta2, mult))))
                       (* handle e => (eprint "here 357\n"; raise e) *)
         in
             A.update (nextdeal, p, Tm.- (now,
                                          Tm.fromMicroseconds us))
             (* print ("next deal " ^ (LargeInt.toString (LargeInt.~ us)) ^
                    " from now\n") *)
         end)
        (* handle e => (eprint "here 312\n"; raise e) *))
    else
        ()

fun checkP ln p =
    if debug then
        let val p' = processorNumber () in
            if p <> p' then
                eprint ("wrong p at " ^ (Int.toString ln) ^ ": " ^
                        (Int.toString p) ^ " != " ^ (Int.toString p') ^ "\n")
            else ()
        end
    else ()


fun addWorkPrio (prio, p, tws) =
    (let (* val _ = block_sigs p *)
        (* val n = queuesWithWork lat *)
        val _ = eprint ("addWorkPrio at " ^ (P.toString prio) ^ "\n")
        val _ = assertblocked "addWorkLat"
        val _ = checkP 271 p
        (* val _ = print ("Added " ^ (if lat then "lat " else "") ^ "work(" ^
                       (Int.toString (List.length tws)) ^ ") on " ^
                       (Int.toString p) ^ "\n") *)
        val q = A.sub (A.sub (!queues, (P.toInt prio) - 1), p)
                handle Subscript => (eprint ("at 414\n"); raise Subscript)
        val _ = ignore (List.map (add q) tws)
        (* val n' = queuesWithWork lat *)
    in
        ()
        (* unblock_sigs p; *)
        (* if n' - n > 1 then
            eprint ("Added more work: n = " ^ (Int.toString n) ^ ", n' = "
                    ^ (Int.toString n') ^ "\n")
        else
            ()
         *)
    end
    ) (* handle e => (eprint "here 341\n"; raise e) *)


fun addWork (p, tws) =
    (assertblocked "addWork";
     print "called at 429\n";
     addWorkPrio (Array.sub (workingOnPriority, p), p, tws))

fun maybeMove {top, bottom, work} =
    let val i = !top
        val j = !bottom
    in
        if i = j then
            (top := 0;
             bottom := 0)
        else
            ()
    end

fun resumeWork (prio, p, (t, w)) =
    (assertblocked "resumeWork";
     print "called at 445\n";
     addWorkPrio (prio, p, [(t, w)]))

fun removeWork (p, Token (tpr, ti, qr)) =
    (block_sigs p;
     (* Array.update (inCriticalSection, p, true); *)
    (case !qr of
        SOME (Queue {top, bottom, work, ...}) =>
        let val tp = !top
            val bot = !bottom
            val w = !work
            fun unloop i =
                if i >= tp - 1 then (top := tp - 1; ())
                else
                    (A.update (w, i, A.sub (w, i + 1));
                     unloop (i + 1))
            fun loop i =
                if i >= tp then false
                else
                    case A.sub (w, i) of
                        Empty => raise WorkQueue
                      | Work (Token (tpr', ti', _), w) =>
                        if tpr = tpr' andalso ti = ti' then
                            (unloop i;
                             (if debug then
                                  ignore (fetchAndAdd (tasksAdded, ~1))
                              else ());
                             true)
                        else
                            loop (i + 1)
        in
            (loop bot) (* handle e => (eprint "here 279\n"; raise e) *)
        end
      | NONE => raise WorkQueue)
    before ((* Array.update (inCriticalSection, p, false); *)unblock_sigs p))

fun removeWorkPrio (_, p, t) = removeWork (p, t)

fun shouldYield _ = false

val policyName = "Prompt private deques"

  val profileDisable = _import "GC_profileDisable" runtime private: unit -> unit;
  val profileEnable = _import "GC_profileEnable" runtime private: unit -> unit;

  val signalThread = _import "signal_thread" runtime private: int * SysWord.word -> unit;

  exception Parallel of string

  val suspends = Array.array (numberOfProcessors, 0)
  fun incSuspends p = Array.update (suspends, p, Array.sub (suspends, p) + 1)

  val delayed = Array.array (numberOfProcessors, nil)

  val enabled = ref true

  fun prerr s = (TextIO.output (TextIO.stdErr, s);
                 TextIO.flushOut TextIO.stdErr)

  fun resumeInt p (Suspend (prio, k), v) =
      let
          val _ = assertblocked "resumeInt"
        val _ = if p = 0 then print "enter resume\n" else ()
        val t = T.prepend (k, fn () => v)
                (* handle e => (eprint "here 117\n"; raise e) *)
      in
        (resumeWork (prio, p, (newWork p, Thread t))
        (* handle e => (eprint "here 59\n"; raise e) *))
        before ((if p = 0 then print "leave resume\n" else ()))
      end
    | resumeInt p (Capture (prio, k), v) =
      let
          val _ = assertblocked "resumeInt"
        (* val _ = print "resuming\n" *)
      in
          print "called at 520\n";
        addWorkPrio (prio, p, [(newWork p, Thread (T.prepend (k, fn () => v)))])
        (* handle e => (eprint "here 67\n"; raise e) *)
      end

  fun resume (t, v) =
      let val p = processorNumber ()
      in
          block_sigs p;
          resumeInt p (t, v);
          unblock_sigs p
      end

  fun addtoio ((t, f) : unit t * (unit -> bool)) =
      let val p = processorNumber ()
          val _ = print ("addtoio at " ^ (Int.toString p) ^ "\n")
          val q = Array.sub (ioqueues, p)
      in
          Array.update (ioqueues, p, (t, f)::q)
      end

  val inpio = ref false

  fun procio p =
      let val _ = assertblocked "procio"
          val _ = checkP 387 p
          (* val _ = if p = 1 then print "enter procio\n" else () *)
          val _ = if p = 0 then ((if !inpio then
                                     print "Entered procio twice!\n"
                                 else ());
                                 inpio := true)
                  else ()
          val q = Array.sub (ioqueues, p)
          val (resumed, q') =
              List.foldl
                  (fn ((t, f), (rsm, r)) =>
                      (* if latency t then *)
                          if f () then (print ("resumed on " ^ (Int.toString p) ^ "\n"); resumeInt p ((* mkLat *) t, ()); (true, r))
                                       (* handle e => (eprint "here 142\n"; raise e) *)
                          else (rsm, (t, f)::r)
                      (* else raise (Parallel "Invariant violated!\n") *)
                  )
                  (false, [])
                  q
      in
          Array.update (ioqueues, p, q');
          resumed
          before (if p = 0 then ((*print "leave procio\n";*) inpio := false) else ())
      end

val waitingForTask = ref false

fun getWorkPrio prioind p =
    let val _ = eprint ("Get work at " ^ (Int.toString prioind) ^ "\n")
        val _ = checkP 513 p
        val q = A.sub (A.sub (!queues, prioind - 1), p)
        val Queue {top, bottom, work, ...} = q
        val i = !top
        val j = !bottom
        val w = !work
    in
        if i = j then NONE
        else
            (checkP 525 p;
             eprint ("got work on " ^ (Int.toString p) ^ "\n");
             SOME (false, P.fromInt prioind, popBottom q))
            (* handle e => (eprint "here 513\n"; raise e) *)
    end

fun getWork p =
    let fun tryGetWork prioind p =
            if prioind <= 0 then NONE
            else
                case getWorkPrio prioind p of
                    NONE => tryGetWork (prioind - 1) p
                  | SOME w => (SOME w before print "got work\n")
    in
        tryGetWork (P.count ()) p
    end

(* handle Range => (eprint "here 588\n"; raise Range) *)

fun startWork p = ()

fun finishWork p = ()

fun checkAndLoop p () =
    let fun loop () = (yield (); loop ())
    in
        (* print ("checking on " ^ (Int.toString p) ^ "\n");*)
        checkP 0 p;
        loop ()
    end

fun suspendWork p = NONE

fun waitForTask' p' =
    (case A.sub (taskcells, p') of
         NONE => ((*print ("waitForTask on " ^ (Int.toString (processorNumber ())) ^ "\n"); *)
                  waitForTask' p')
       | SOME bw => bw
                    before ((* eprint ("got a task from " ^ (Int.toString p') ^ "\n"); *)
                            A.update (taskcells, p', NONE)))
    (* handle e => (eprint "here 384\n"; raise e) *)

fun makeRequest p =
    let val mi = case Int.maxInt of SOME i => i | NONE => 1000000
        val r = Word.mod (MLton.Random.rand (), Word.fromInt 1000000)
        val rnd = (Real./
                   (Real.fromInt ((Word.toInt r) + 1), 1000001.0))
        (* handle e => (eprint "here 171\n"; raise e) *)
        val mult = (Math.ln rnd)
        (* handle e => (eprint "here 353\n"; raise e) *)
        val us = (LargeInt.fromInt
                      (Real.floor (Real.* (Real.fromInt delta2, mult))))
        val p' =
            (p + 1 + ((Word.toIntX (MLton.Random.rand ())) mod (P - 1))) mod P
            (* handle e => (eprint "here 695\n"; raise e) *)
        val c = V.sub (reqcells, p')
    in
        c := p;
        Tm.- (Tm.now (), Tm.fromMicroseconds us)
    end

fun tryToUpdate p c v =
    let val ev = !c
        val _ = assertblocked "tryToUpdate"
        (* val _ = print ((Int.toString p) ^ "'s cell is " ^ (Int.toString ev) ^ "\n") *)
        in
        if ev > notwaiting then
            let val (nlat, w) = waitForTask' ev
            in
                print "called at 652\n";
                addWorkPrio (nlat, p, [(newWork p, w)]);
                c := v
            end
        else
            if ev = v then ()
            else
                ((* print ("Hopefully " ^ (Int.toString (!c)) ^ " <= " ^
                        (Int.toString notwaiting) ^ "\n"); *)
                 if compareAndSwap (c, ev, v) then
                     ()
(*
                     print ("cas(" ^ (Int.toString (refToInt c)) ^ ", " ^
                            (Int.toString ev) ^ ", " ^
                            (Int.toString v) ^ ") succeeded (" ^
                            (Int.toString p) ^ ")\n")
*)
(*
                 eprint ((Int.toString p) ^ "'s cell was " ^
                         (Int.toString ev) ^ ", now " ^
                         (Int.toString (!c)) ^ "\n")
*)
                 else
                     tryToUpdate p c v)
    end
(*
        else
            raise (Parallel "comm cell updated to non-processor value")
*)

  fun schedule countSuspends k () =
    let
        fun busyloop () =
            (yield ();
             busyloop ())
        fun loop (countSuspends, k, p, nextReq) =
          let
              fun setLFW () = Array.update (lookingForWork, p, true)
              fun clearLFW () = Array.update (lookingForWork, p, false)
              val lfw = Array.sub (lookingForWork, p)
              val prio = Array.sub (workingOnPriority, p)
              val _ = setLFW ()
              (* val p = processorNumber () *)
              val _ = block_sigs p
              val _ = MLton.Thread.atomically (fn () => procio p)
              val _ = communicate p
              val c = V.sub (commcells, p)
              val p' = !c
              val _ = ((* print ("Call site 1 on " ^ (Int.toString p) ^ ". Mine is " ^
                               (Int.toString (!c)) ^ "\n"); *)
                       if p' > notwaiting then
                          let val (nlat, w) = waitForTask' p'
                          in
                              assertblocked "scheduler793";
                              print "called at 706\n";
                              addWorkPrio (nlat, p, [(newWork p, w)]);
                              c := notwaiting
                          end
                       else
                           ())
              (* val _ = Array.update (lookingForWork, p, true) *)
(*
              val _ = Array.update (inCriticalSection, p, false)
              val _ = if p = 0 then (unblock_sigs p;
                                     block_sigs p) else ()
              val _ = Array.update (inCriticalSection, p, true)
*)
              val _ = checkP 523 p
              val _ =
                  case k of
                      NONE =>  ()
                    | SOME t =>
                      if not lfw then
                          (print "pushing continuation\n";
                           assertblocked "scheduler813";
                           print "called at 727";
                           addWorkPrio (prio, p, [(newWork p, RThread (t, p))]))
                      else print "was looking for work\n"
          in
            case (checkP 526 p;
                                 getWork p (* handle e => (eprint "here 483\n";
                                                        raise e) *))
                                before
                                ((* print "got work\n"; *)
                                  checkP 531 p (*;
                                  (*  clearLFW ();*)
                                 print "updated\n" *))
             of NONE =>
                let (* val _ = print "didn't actually get work\n" *) in
                  (* if !enabled then (enabled := false; profileDisable ()) else (); *)
                    (*print ("Call site 2 on " ^ (Int.toString p) ^ ". Mine is " ^
                            (Int.toString (!c)) ^ "\n"); *)
                    (* block_sigs p;*)
                    tryToUpdate p c notask;
                    assertblocked "scheduler824";
                    unblock_sigs p;
                    if P > 1 andalso Time.> (Time.now (), nextReq) then
                        loop (countSuspends, NONE, p, makeRequest p)
                    else loop (countSuspends, NONE, p, nextReq)
                end
              | SOME (nonlocal, prio, j) =>
                let
                    val _ = checkP 541 p
                    val _ = clearLFW ()
                    val _ = if debug then ignore (fetchAndAdd (tasksRun, 1))
                            else ()
                    (* val p = processorNumber () *)
                  (* val _ = print ("starting " ^ (if nonlocal then "nonlocal "
                                                else "") ^ "thread on "
                                 ^ (Int.toString p) ^ "\n") *)
(*                    val () = print ("updating latency to " ^
                                    (if lat then "true" else "false") ^ "\n") *)
                    val () = Array.update (workingOnPriority, p, prio)
                    (* val () = print ("Call site 3 on " ^ (Int.toString p) ^ ". Mine is " ^
                            (Int.toString (!c)) ^ "\n") *)
                    (* val () = block_sigs p *)
                    val () = tryToUpdate p c (flag_of_prio prio)
                    val () = unblock_sigs p
                  val () = if countSuspends andalso nonlocal then incSuspends p else ()
                  (* val () = if not (!enabled) then (enabled := true; profileEnable ()) else (); *)
                  val () = startWork p
                  (* val () = Array.update (inCriticalSection, p, false) *)
                  (* val _ = unblock_sigs p *)
                  val () = (case j
                             of JWork w => (print "starting JWork\n";
                                            w ())
                              | Thread k => (print "starting Thread\n";
                                             (T.switch (fn _ => (T.prepare (k, ()))))
                                            (* handle e => (eprint "here 472\n";
                                                         raise e) *))
                              | RThread (r, p') => (print "starting RThread\n";
                                                    (if p' <> p then
                                                         print ("RThread was stolen from " ^ (Int.toString p') ^ ". I'm " ^ (Int.toString p) ^ "\n")
                                                     else ());
                                              T.switch (fn _ => (r))
                                              (* handle e => (eprint "here 512\n";
                                                         raise e) *)))
                      (* PERF? this handle only makes sense for the Work case *)
                      (* PERF? move this handler out to the native entry point? *)
                      handle Parallel s =>
                             (TextIO.output (TextIO.stdErr,
                                             ("WARNING: Caught parallel exception \""
                                                   ^ s
                                                   ^ "\" in parallel scheduler!\n"));
                                   TextIO.flushOut TextIO.stdErr;
                                   OS.Process.exit OS.Process.failure)
                             | e => (TextIO.output (TextIO.stdErr,
                                                  ("WARNING: Caught exception \""
                                                   ^ (General.exnMessage e)
                                                   ^ "\" in parallel scheduler!\n"));
                                   TextIO.flushOut TextIO.stdErr;
                                   OS.Process.exit OS.Process.failure)
                  (* A job should never return -- we will only get here in exceptional
                    conditions. *)
                  (* NB we call processorNumber again here in case that this
                    job has been split across two processors *)
                  val p = processorNumber ()
                  val () = incSuspends p
                  val () = finishWork p
                in
                  loop (false, NONE, p, nextReq)
                end
          end
        val p = processorNumber ()
    in
        (* checkAndLoop (processorNumber ()) () *)
        (* block_sigs p;*)
        (* Array.update (inCriticalSection, p, true); *)
        loop (countSuspends, k, p, Time.zeroTime)
        (* busyloop (processorNumber ()) 0 *)
    end

  fun capture' (p, tail) =
      let val _ = print "capture'\n"
          val _ = assertblocked "capture'"
      in
      T.switch
          (fn k =>
              (* Note that we cannot call addWork on the current thread!
                Also, we can't call directly schedule here because we need to
                preserve the current thread/stack. Instead we switch to a
                different thread that will continue by calling f and then
                schedule.  This avoids a whole host of bugs related to f
                leaking out k to another thread and that thread resuming k
                before f has finished. *)
              let
                (* Check to see what the next job is.  If it's a thread, then
                  hijack that thread to run the tail of the current job.
                  Otherwise, create a new thread. *)
                (*
                  val t =
                    case getWork p
                     of SOME (_, _, JWork w) => T.new (fn () => (startWork p; w ()))
                      | SOME (_, _, Thread k') =>
                        (print "before-prepend\n";
                         T.prepend (k', fn () => (startWork p))
                         before print "after-prepend\n")
                      | SOME (_, _,  RThread (r, _)) =>
                        T.new (fn () => (startWork p; T.switch (fn _ => r)))
                      | NONE => T.new (schedule false NONE)
                *)
                (* to disable hijacking, use this instead *)
                  val _ = Array.update (lookingForWork, p, true)
                  val _ = Array.update (workingOnPriority, p, P.bot)
                val t = T.new (schedule false NONE)
                fun add (prio, w) =
                    (assertblocked "capture'-add";
                     addWorkPrio (prio, p, [(newWork p, JWork w)]))
              in
                (* XX maybe this should move out (before suspend/finishWork) *)
                (* add any delayed work *)
                app add (rev (Array.sub (delayed, p)));
                Array.update (delayed, p, nil);
                (* return the new thread to switch to *)
                print "preparing\n";
                (T.prepare (T.prepend (t, tail), (p, k))
                (* handle e => (eprint "here 176\n"; raise e) *))
              end)
      end

  fun suspend f =
      let
        val p = processorNumber ()
        (* Block signals so we don't preempt while we are suspending *)
        val () = block_sigs p
        (* val _ = Array.update (inCriticalSection, p, true) *)
        val prio = Array.sub (workingOnPriority, p)
        val _ = print "suspend\n"
        (* val _ = print ("suspend at " ^ (Int.toString p) ^ "\n") *)
        fun tail (p, k) =
            let
              val () = incSuspends p
              val q = suspendWork p
            in
              f (Suspend (prio, k))
              before (unblock_sigs p)
            end
      in
        capture' (p, tail)
      end

  fun capture f =
      let
        val p = processorNumber ()
        val () = block_sigs p
        (* val _ = print "capture\n" *)
        fun tail (p, k) =
            let
              val () = incSuspends p
              val () = finishWork p
            in
              f (Capture (P.bot, k)) (* XXX *)
              before (unblock_sigs p)
            end
      in
        capture' (p, tail)
      end

  fun yield () =
      let
        val p = processorNumber ()
      in
          (*
        if shouldYield p then
          capture' (p, fn (p, k) =>
                          let in
                            addWorkLat (false, p, [(newWork p, Thread k)]); (* XXX *)
                            incSuspends p;
                            finishWork p
                          end)
        else *)
          ()
      end

  fun addRightPrio (prio, w) =
      let
        val p = processorNumber ()
        val _ = block_sigs p
        val _ = assertblocked ("addRightLat1044 " ^ (Int.toString p))
        (* val _ = Array.update (inCriticalSection, p, true) *)
        val _ = assertblocked "addRightLat1046"
        val t = newWork p
        val _ = assertblocked "addRightLat1048"
        val sw = ref false
        val _ = assertblocked "addRightLat1050"
        fun w' () = if !sw then raise Parallel "work run twice"
                    else (sw := true; w ())

      in
        if shouldYield p then
          (* Switch to a new thread *)
          capture' (p, fn (p, k) =>
                         let in
                           (* Add the continuation first -- it is higher priority *)
                             assertblocked "addRightLat-f";
                             print "called at 947\n";
                           addWorkPrio (prio, p, [(newWork p, Thread (T.prepend (k, fn () => t))),
                                          (t, JWork w')])
                           (* handle e => (eprint "here 622\n"; raise e) *);
                           incSuspends p;
                           finishWork p
                         end)
        else
          let
              val _ = assertblocked "addRightLat1069"
            fun add (prio, w) =
                (assertblocked "addRightLat-add";
                 print "called at 959\n";
                 addWorkPrio (prio, p, [(newWork p, JWork w)]))
          in
            (* add any delayed work *)
            (* XXX maybe should run delayed work and queue the currrent thread too? *)
            app add (rev (Array.sub (delayed, p)));
            Array.update (delayed, p, nil);
            assertblocked "addRightLat";
            print "called at 967";
            addWorkPrio (prio, p, [(t, JWork w')]);
            (* Array.update (inCriticalSection, p, false); *)
            unblock_sigs p;
            t
          end
      end

  fun addRight w = addRightPrio (P.bot, w)

                               (*
  (* smuller: XXX This adds the current thread to the lqueue and so is totally
     wrong. Fix it. *)
  fun addLeftLat (lat, w) =
      let
        val p = processorNumber ()
        val t = newWork p
        val _ = block_sigs p
      in
        if shouldYield p then
          capture' (p, fn (p, k) =>
                         let in
                           addWorkLat (lat, p, [(newWork p, JWork w),
                                          (t, Thread (T.prepend (k, fn () => t)))])
                         (* handle e => (print "here 654\n"; raise e) *);
                           incSuspends p;
                           finishWork p
                         end)
        else
          T.switch (fn k =>
                       T.prepare
                       (T.new (fn () =>
                                  let
                                    fun add (lat, w) = addWorkLat (lat, p, [(newWork p, JWork w)])
                                  in
                                    (* add any delayed work *)
                                    (* XXX maybe should run delayed work and queue the currrent thread too? *)
                                    app add (rev (Array.sub (delayed, p)));
                                    Array.update (delayed, p, nil);
                                    addWorkLat (lat, p, [(t, Thread (T.prepend (k, fn () => t)))])
                                    (* handle e => (print "here 670\n"; raise e) *);
                                    unblock_sigs p;
                                    w ()
                                  end), ()))
      end

  fun addLeft w =
      let
        val p = processorNumber ()
        val t = newWork p
        val _ = block_sigs p
        val _ = print "addLeft\n"
      in
        if shouldYield p then
          capture' (p, fn (p, k) =>
                         let in
                           addWork (p, [(newWork p, JWork w),
                                          (t, Thread (T.prepend (k, fn () => t)))])
                         (* handle e => (print "here 654\n"; raise e) *);
                           incSuspends p;
                           finishWork p
                         end)
        else
          T.switch (fn k =>
                       T.prepare
                       (T.new (fn () =>
                                  let
                                    fun add (lat, w) = addWork (p, [(newWork p, JWork w)])
                                  in
                                    (* add any delayed work *)
                                    (* XXX maybe should run delayed work and queue the currrent thread too? *)
                                    app add (rev (Array.sub (delayed, p)));
                                    Array.update (delayed, p, nil);
                                    addWork (p, [(t, Thread (T.prepend (k, fn () => t)))])
                                    (* handle e => (print "here 670\n"; raise e) *);
                                    unblock_sigs p;
                                    w ()
                                  end), ()))
      end
*)

  fun removePrio prio t = removeWorkPrio (prio, processorNumber (), t)
  fun remove t = removeWork (processorNumber (), t)

(* XXX left? what about the right? *)
  fun delayedAddPrio (prio, w) =
      let
        (* PERF use a array-based buffer to avoid allocation *)
        val p = processorNumber ()
        val ws = Array.sub (delayed, p)
      in
        Array.update (delayed, p, (prio, w)::ws)
      end

  fun delayedAdd w = delayedAddPrio (P.bot, w)

  fun return () =
      let
        val p = processorNumber ()
        val _ = block_sigs p
      in
        (* Look for delayed work *)
        case Array.sub (delayed, p)
         of nil => ((* this is counted in schedule: incSuspends p;  *)
                    finishWork p;
                    unblock_sigs p;
                    schedule true NONE ())
          | ws =>
            let
              val ((prio, w), ws) =
                  case rev ws of (prio, w)::ws => ((prio, w), ws) | nil => raise Match
              val () = Array.update (delayed, p, nil)
              fun add nil = ()
                | add ((prio, w)::ws) =
                  addWorkPrio (prio, p, [(newWork p, JWork (fn () => (add ws; w ())))])
              (* add any lower priority work *)
              val () = add ws
            in
              (* now what do to with w? *)
              if shouldYield p then
                (addWorkPrio (P.bot, p, [(newWork p, JWork w)]); (* XXX *)
                 (* this is counted in schedule: incSuspends p; *)
                 finishWork p;
                 unblock_sigs p;
                 schedule true NONE ())
              else
                w ()
            end
      end

  fun signalOthers p sg n =
      if n = numberOfProcessors then ()
      else if n = p then signalOthers p sg (n + 1)
      else
          (print ("signaling" ^ (Int.toString n) ^ "\n");
           ((signalThread (n, Posix.Signal.toWord sg);
             print "signaled\n")
           (* handle _ => print "signal failed\n" *));
           signalOthers p sg (n + 1))

  fun signal2Others p sg =
      let val p1 = 2 * (p + 1) - 1
          val p2 = 2 * (p + 1)
      in
          (if p1 < P then
               signalThread (p1 - 1, Posix.Signal.toWord sg)
           else ());
          (if p2 < P then
               signalThread (p2 - 1, Posix.Signal.toWord sg)
           else ())
      end

  fun countWork prio =
    let fun cWork p =
            let val q = A.sub (A.sub (!queues, (P.toInt prio) - 1), p)
                val Queue {top, bottom, ...} = q
                val i = !top
                val j = !bottom
            in
                i - j
            end
        fun add (a, b) = a + b
    in
        List.foldl add 0 (List.tabulate (P, fn i => cWork i))
    end


  fun interruptFst t =
      let val p = processorNumber ()
          (* val _ = MLton.Itimer.set (MLton.Itimer.Real,
                                   {interval = Time.zeroTime,
                                    value = Time.zeroTime}) *)
          val _ = if !waitingForTask then print "interrupt while waiting!!!\n"
                  else ()
          (* val _ = block_sigs p *)
          val iv = Time.fromMicroseconds (LargeInt.fromInt delta)
          (* val _ = procio p *)
          (* val _ = communicate p *)
      in
       (
         print ("interruptFst on " ^ (Int.toString p) ^ "\n");
         signalOthers p sec_sig 1;
         (* (if debug andalso (Array.all (fn x => not x) inCriticalSection) then
              let val _ =
                      if (queuesWithWork false) + (queuesWithWork true) = 0 then
                          eprint "There's no work left!\n"
                      else ()
                  val shouldBe = !tasksAdded (* - !tasksRun *)
                  val thereAre = (countWork true) + (countWork false)
              in
                  if shouldBe <> thereAre then
                      eprint ("Work was lost. There should be " ^
                              (Int.toString shouldBe) ^ ". There are " ^
                              (Int.toString thereAre) ^ ".\n")
                  else
                      ()
              end
          else ()); *)
         MLton.Itimer.set (MLton.Itimer.Real,
                          {interval = Time.zeroTime, value = iv});
         (* signal2Others p sec_sig; *)
         (* signalThread (0, Posix.Signal.toWord sec_sig); *)
         (* unblock_sigs p; *)
         (* print ("signaled others\n"); *)
         (if Array.sub (inCriticalSection, p) then t
          else
              T.prepare (T.new (schedule true (SOME t)), ())))
      (* T.prepare (T.new (checkAndLoop p), ())) *)
      end

  fun interrupt t =
      let val p = processorNumber ()
          (* val _ = procio p *)
          (*val _ = communicate p *)
      in
       (print ("interrupt on " ^ (Int.toString p) ^ "\n");
        if Array.sub (inCriticalSection, p) then t
        else
         (* signal2Others p sec_sig; *)
            ((* block_sigs p;*)
             T.prepare (T.new (schedule true (SOME t)), ())))
        (* if p = 1 then t (* *)
        else
            T.prepare (T.new (checkAndLoop p), ())) *)
      end

  fun simple () =
      print "interrupt\n"

  val p = processorNumber ()

 (*  val _ = S.setHandler (sg, (S.Handler.ignore)) *)

  val procsInit = ref 0

  fun initProc () =
      let val p = processorNumber ()
          val pi = fetchAndAdd (procsInit, 1)
          (* val _ = print ("setting handler on " ^ (Int.toString p) ^ "\n") *)
          val _ = S.setHandler (prim_sig, (S.Handler.handler interruptFst))
          val _ = S.setHandler (sec_sig, (S.Handler.handler interrupt))
          (* val _ = print ("in init " ^ (Int.toString p) ^ "\n") *)
          val iv = Time.fromMicroseconds (LargeInt.fromInt delta)
          val _ = MLton.Random.srand 0w42
      in
          (if p = 0 then
               (
               (* (MLton.Random.srand
                      (case MLton.Random.useed () of
                           SOME s => s
                         | NONE => (eprint "Warning: RNG not seeded properly";
                                    0w0)); *)
                 S.Mask.unblock (S.Mask.some [prim_sig]);
                 S.Mask.block (S.Mask.some [sec_sig]);
(*
                punblocksig prim_sig;
                pblocksig sec_sig;
*)
                Array.update (lookingForWork, p, false))
           else
               (S.Mask.block (S.Mask.some [prim_sig]);
                S.Mask.unblock (S.Mask.some [sec_sig])
(*
                pblocksig prim_sig;
                punblocksig sec_sig *)
               )
          );
          (if pi = P - 1 then
               MLton.Itimer.set (MLton.Itimer.Real,
                                 {interval = Tm.zeroTime, value = iv})
           else
               ())
          (* eprint ("initialized " ^ (Int.toString p) ^ "\n") *)
          (* schedule false () *)
      end

  fun prun () =
      (initProc ();
       (* print ("initialized; starting"); *)
       schedule false NONE ())

                                               (*
  val () = (_export "Parallel_run": (unit -> void) -> unit;) (prun)
  val () = (_export "Parallel_sched_init": (unit -> void) -> unit;) (init)
  (* init MUST come after schedulerLoop has been exported *)
  val () = (_import "Parallel_init" runtime private: unit -> unit;) ()
*)

  val policyName = policyName
  val maxBytesLive = _import "Parallel_maxBytesLive" runtime private: unit -> Word64.word;
  val gcTime = _import "Parallel_getTimeInGC" runtime private: unit -> Word64.word;
  val successfulSteals = reportSuccessfulSteals
  val failedSteals = reportFailedSteals
  fun resetStatistics () =
      let
        val resetBytesLive = _import "Parallel_resetBytesLive" runtime private: unit -> unit;
      in
        resetSteals ();
        Array.modify (fn _ => 0) suspends;
        resetBytesLive ()
      end
  val suspends = fn () => Array.foldl op+ 0 suspends

  fun finalizePriorities () =
      (P.init ();
       eprint "initialized\n")

  fun init () =
      (eprint "checking\n";
       P.check ();
       eprint "checked\n";
       queues := A.tabulate (P.count (),
                             fn i => A.tabulate (P, fn _ => newQueue (P.fromInt (i + 1))));
       eprint ("initialized " ^ (Int.toString (A.length (!queues))) ^ " queues\n");
       MLton.Parallel.registerProcessorFunction (prun);
       initProc ();
       MLton.Parallel.initializeProcessors ())

end
