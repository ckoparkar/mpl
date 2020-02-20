datatype tree = Leaf of int
              | Node of int * tree * tree;

fun sumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => n + (sumtree x) + (sumtree y)

fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun sbuildtree n =
  if n <= 0
  then Leaf (sfib (20))
  else
    let val (x, y) = (sbuildtree (n-1), sbuildtree (n-1))
    in Node (n, x, y)
    end

fun buildtree n =
  if n <= 0
  then Leaf (sfib (20))
  else
  if n < 19
  then sbuildtree n
  else
    let val (x, y) = ForkJoin.par(fn _ => buildtree (n-1), fn _ => buildtree (n-1))
    in Node (n, x, y)
    end

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = Bench.print_bench "parbuildfib" iters buildtree size
val _ = print (Int.toString (sumtree tr) ^ "\n")
