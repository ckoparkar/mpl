datatype tree = Leaf of int
              | Node of int * tree * tree;

fun sbuildtree n =
  if n <= 0
  then Leaf (6765)
  else
    let val (x, y) = (sbuildtree (n-1), sbuildtree (n-1))
    in Node (n, x, y)
    end

fun buildtree n =
  if n <= 0
  then Leaf (6765)
  else
  if n < 19
  then sbuildtree n
  else
    let val (x, y) = ForkJoin.par(fn _ => buildtree (n-1), fn _ => buildtree (n-1))
    in Node (n, x, y)
    end

fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => n + (ssumtree x) + (ssumtree y)

fun sumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) =>
    let
      val (i,j) =  if n < 19
                   then ((ssumtree x), (ssumtree y))
                   else ForkJoin.par((fn _ => sumtree x), (fn _ => sumtree y))
    in
      n + i + j
    end

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = buildtree size
val sum = Bench.print_bench "parsumpartree" iters (fn _ => sumtree tr) size
val _ = print (Int.toString sum ^ "\n")
