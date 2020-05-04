structure W = Word

fun sfib n =
    if W.<= (n, W.fromInt 1)
    then n
    else W.+ (sfib (W.- (n, W.fromInt 1)),
              sfib (W.- (n, W.fromInt 2)))
fun fib cutoff n =
    if W.<= (n, cutoff)
    then sfib n
    else
      let
        val (x,y) = ForkJoin.par (fn _ => fib cutoff (W.- (n, W.fromInt 1)),
                                  fn _ => fib cutoff (W.- (n, W.fromInt 2)))
      in
        W.+ (x, y)
      end
