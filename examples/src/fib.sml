fun sfib n =
    if n <= 1
    then n
    else (sfib (n-1)) + (sfib (n-2))

fun fib cutoff n =
    if n <= 1
    then n
    else if n <= cutoff
    then sfib n
    else
      let
        val (x,y) = ForkJoin.par (fn _ => fib cutoff (n-1),
                                  fn _ => fib cutoff (n-2))
      in
        x + y
      end
