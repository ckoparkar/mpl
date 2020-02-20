fun sfib n =
    if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun fib n =
    if n <= 20 then sfib n
    else
      let
        val (x,y) = ForkJoin.par (fn _ => fib (n-1), fn _ => fib (n-2))
      in
        x + y
      end

val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val _ = Bench.print_bench "parfib" iters fib size
