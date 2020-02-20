datatype tree = Leaf of int
              | Node of int * tree * tree;

fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun sbuildtree n =
  if n <= 0
  then Leaf (sfib 20)
  else
    let val (x, y) = (sbuildtree (n-1), sbuildtree (n-1))
    in Node (n, x, y)
    end

fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => n + (ssumtree x) + (ssumtree y)

fun sadd1tree tr =
  case tr of
    Leaf (i)       => Leaf(i+1)
  | Node (n, x, y) =>
    let
      val (a,b) = (sadd1tree x, sadd1tree y)
    in
      Node(n+1, a, b)
    end

fun add1tree tr =
  case tr of
    Leaf (i)       => Leaf(i+1)
  | Node (n, x, y) =>
    let
      val (a,b) = if n < 19
                  then (sadd1tree x, sadd1tree y)
                  else ForkJoin.par((fn _ => add1tree x), (fn _ => add1tree y))
    in
      Node(n+1, a, b)
    end

val size = CommandLineArgs.parseInt "N" 25
val iters = CommandLineArgs.parseInt "I" 9
val tr = sbuildtree size
val tr2 = Bench.print_bench "paradd1tree" iters (fn _ => add1tree tr) size
val _ = print (Int.toString (ssumtree tr2) ^ "\n")
