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

fun bench msg f n =
  let
    val _ = print (msg ^ Int.toString n ^ "\n")
    val t0 = Time.now ()
    val result = f n
    val t1 = Time.now ()
    val _ = print ("finished in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")
    val _ = print ("result " ^ Int.toString result ^ "\n")
  in
    result
  end

val n = CommandLineArgs.parseInt "N" 39
val _ = bench "fib " fib n
val _ = bench "sfib " sfib n
