structure W = Word

fun sfib n =
    if W.<= (n, W.fromInt 1)
    then n
    else W.+ (sfib (W.- (n, W.fromInt 1)),
              sfib (W.- (n, W.fromInt 2)))
fun fib n =
    if W.<= (n, W.fromInt 19)
    then sfib n
    else
      let
        val (x,y) = ForkJoin.par (fn _ => fib (W.- (n, W.fromInt 1)),
                                  fn _ => fib (W.- (n, W.fromInt 2)))
      in
        W.+ (x, y)
      end

val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val n = Bench.print_bench "parfib" iters fib (W.fromInt size)
val _ = print (Int.toString (W.toInt n) ^ "\n")
