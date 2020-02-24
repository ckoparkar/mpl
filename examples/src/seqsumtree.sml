datatype tree = Leaf of int
              | Node of int * tree * tree;

fun sbuildtree n =
  if n <= 0
  then Leaf 6765
  else
    let val (x, y) = (sbuildtree (n-1), sbuildtree (n-1))
    in Node (n, x, y)
    end

fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => n + (ssumtree x) + (ssumtree y)

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = sbuildtree size
val sum = Bench.print_bench "seqsumtree" iters (fn _ => ssumtree tr) size
val _ = print (Int.toString sum ^ "\n")
