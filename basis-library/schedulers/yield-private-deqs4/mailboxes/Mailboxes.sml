structure Mailboxes :> MAILBOXES =
struct

  exception Mailboxes

  val myMutexLock   = _import "Parallel_myMutexLock"   runtime private : int -> unit;
  val myMutexUnlock = _import "Parallel_myMutexUnlock" runtime private : int -> unit;
  val myCondSignal  = _import "Parallel_myCondSignal"  runtime private : int -> unit;
  val myMailLoop    = _import "Parallel_myMailLoop"    runtime private : int ref * int -> unit;

  val P = MLton.Parallel.numberOfProcessors
  val vcas = MLton.Parallel.compareAndSwap
  fun cas (r, old, new) = vcas r (old, new) = old

  type 'a mailbox = {flag : int ref, mail : 'a ref}
  type 'a t = 'a mailbox vector

  val MAIL_WAITING = 0
  val MAIL_RECEIVING = 1

  fun new d =
    Vector.tabulate (P, fn _ => {flag = ref MAIL_WAITING, mail = ref d})

  val YIELD_CNT = 32
  fun modY x = if x >= YIELD_CNT then x - YIELD_CNT else x
  
  fun getMail mailboxes p =
    let
      val {flag, mail} = Vector.sub (mailboxes, p)
    in
      (
       myMailLoop(flag, YIELD_CNT-1);
       !mail
      )
    end

  fun sendMail mailboxes (p, m) =
    let
      val {flag, mail} = Vector.sub (mailboxes, p)
      val _ = myMutexLock(p)
    in ( mail := m
       ; if cas (flag, MAIL_WAITING, MAIL_RECEIVING) then (
          myMutexUnlock(p);
          myCondSignal (p)
         )
         else raise Mailboxes
       )
    end

  fun sendMailLockFree mailboxes (p, m) =
    let
      val {flag, mail} = Vector.sub (mailboxes, p)
    in ( mail := m
       ; if cas (flag, MAIL_WAITING, MAIL_RECEIVING) then ()
         else raise Mailboxes
       )
    end
end