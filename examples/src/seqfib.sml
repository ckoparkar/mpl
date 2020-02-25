structure W = Word

fun sfib n =
    if W.<= (n, W.fromInt 1)
    then n
    else W.+ (sfib (W.- (n, W.fromInt 1)),
              sfib (W.- (n, W.fromInt 2)))

val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val n = Bench.print_bench "seqfib" iters sfib (W.fromInt size)
val _ = print (Int.toString (W.toInt n) ^ "\n")
