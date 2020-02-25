structure W = Word

datatype tree = Leaf of W.word
              | Node of W.word * tree * tree;

fun sfib n =
    if W.<= (n, W.fromInt 1)
    then n
    else W.+ (sfib (W.- (n, W.fromInt 1)),
              sfib (W.- (n, W.fromInt 2)))

fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => W.+ (W.+ (n, (ssumtree x)), (ssumtree y))


fun sbuildfib n =
  if W.<= (n, (W.fromInt 0))
  then Leaf (sfib (W.fromInt 20))
  else
    let val (x, y) = (sbuildfib (W.- (n, W.fromInt 1)),
                      sbuildfib (W.- (n, W.fromInt 1)))
    in Node (n, x, y)
    end

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = Bench.print_bench "seqbuildfib" iters sbuildfib (W.fromInt size)
val _ = print (Int.toString (W.toInt (ssumtree tr)) ^ "\n")
