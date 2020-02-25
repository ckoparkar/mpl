structure W = Word

datatype tree = Leaf of W.word
              | Node of W.word * tree * tree;

fun sbuildtree n =
  if W.<= (n, (W.fromInt 0))
  then Leaf (W.fromInt 6765)
  else
    let val (x, y) = (sbuildtree (W.- (n, W.fromInt 1)),
                      sbuildtree (W.- (n, W.fromInt 1)))
    in Node (n, x, y)
    end

fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => W.+ (W.+ (n, (ssumtree x)), (ssumtree y))

fun sadd1tree tr =
  case tr of
    Leaf (i)       => Leaf (W.+(i, W.fromInt 1))
  | Node (n, x, y) =>
    let
      val(a,b) = (sadd1tree x, sadd1tree y)
    in
      Node((W.+(n, W.fromInt 1)), a, b)
    end

fun add1tree tr =
  case tr of
    Leaf (i)       => Leaf (W.+ (i, W.fromInt 1))
  | Node (n, x, y) =>
    let
      val (a,b) = if W.< (n, W.fromInt 19)
                  then (sadd1tree x, sadd1tree y)
                  else ForkJoin.par((fn _ => add1tree x), (fn _ => add1tree y))
    in
      Node(W.+ (n, W.fromInt 1), a, b)
    end

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = sbuildtree (W.fromInt size)
val tr2 = Bench.print_bench "paradd1tree" iters (fn _ => add1tree tr) (W.fromInt size)
val _ = print (Int.toString (W.toInt (ssumtree tr2)) ^ "\n")
