structure W = Word

datatype tree = Leaf of W.word
              | Node of W.word * tree * tree;

(* build tree *)
fun sbuildtree n =
  if W.<= (n, (W.fromInt 0))
  then Leaf (W.fromInt 6765)
  else
    let val (x, y) = (sbuildtree (W.- (n, W.fromInt 1)),
                      sbuildtree (W.- (n, W.fromInt 1)))
    in Node (n, x, y)
    end

fun buildtree cutoff n =
  if W.<= (n, cutoff)
  then sbuildtree n
  else
    let val (x, y) = ForkJoin.par ((fn _ => buildtree cutoff (W.- (n, W.fromInt 1))),
                                   (fn _ => buildtree cutoff (W.- (n, W.fromInt 1))))
    in Node (n, x, y)
    end

(* add1 tree *)

fun sadd1tree tr =
  case tr of
    Leaf (i)       => Leaf (W.+(i, W.fromInt 1))
  | Node (n, x, y) =>
    let
      val(a,b) = (sadd1tree x, sadd1tree y)
    in
      Node((W.+(n, W.fromInt 1)), a, b)
    end

fun add1tree cutoff tr =
  case tr of
    Leaf (i)       => Leaf (W.+ (i, W.fromInt 1))
  | Node (n, x, y) =>
    let
      val (a,b) = if W.< (n, cutoff)
                  then (sadd1tree x, sadd1tree y)
                  else ForkJoin.par((fn _ => add1tree cutoff x), (fn _ => add1tree cutoff y))
    in
      Node(W.+ (n, W.fromInt 1), a, b)
    end

(* sum tree *)
fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => W.+ (W.+ (n, (ssumtree x)), (ssumtree y))

fun sumtree cutoff tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) =>
    let
      val (i,j) =  if W.< (n, (W.fromInt 19))
                   then ((ssumtree x), (ssumtree y))
                   else ForkJoin.par((fn _ => sumtree cutoff x), (fn _ => sumtree cutoff y))
    in
      W.+ (W.+ (n, i), j)
    end


(* build fib *)
fun sbuildfib n =
  if W.<= (n, (W.fromInt 0))
  then Leaf (sfib (W.fromInt 20))
  else
    let val (x, y) = (sbuildfib (W.- (n, W.fromInt 1)),
                      sbuildfib (W.- (n, W.fromInt 1)))
    in Node (n, x, y)
    end

fun buildfib cutoff n =
  if W.<= (n, W.fromInt 0)
  then Leaf (sfib (W.fromInt 20))
  else
  if W.< (n, cutoff)
  then sbuildfib n
  else
    let val (x, y) = ForkJoin.par(fn _ => buildfib cutoff (W.- (n, W.fromInt 1)),
                                  fn _ => buildfib cutoff (W.- (n, W.fromInt 1)))
    in Node (n, x, y)
    end
