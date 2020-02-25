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

fun buildtree n =
  if W.<= (n, W.fromInt 0)
  then Leaf (W.fromInt 6765)
  else
  if W.< (n, W.fromInt 19)
  then sbuildtree n
  else
    let val (x, y) = ForkJoin.par(fn _ => buildtree (W.- (n, W.fromInt 1)),
                                  fn _ => buildtree (W.- (n, W.fromInt 1)))
    in Node (n, x, y)
    end

fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => W.+ (W.+ (n, (ssumtree x)), (ssumtree y))

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = buildtree (W.fromInt size)
val sum = Bench.print_bench "seqsumpartree" iters (fn _ => ssumtree tr) (W.fromInt size)
val _ = print (Int.toString (W.toInt sum) ^ "\n")
