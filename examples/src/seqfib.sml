fun sfib n =
    if n <= 1 then n else sfib (n-1) + sfib (n-2)

val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val n = Bench.print_bench "seqfib" iters sfib size
val _ = print (Int.toString n ^ "\n")
