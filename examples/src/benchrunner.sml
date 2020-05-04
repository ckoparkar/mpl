fun str_eq s1 s2  =
    case String.compare (s1, s2) of
        EQUAL => true
      | _     => false

fun run prog size iters =
    case prog of
        "seqfib" =>
            let
                val n = Bench.print_bench prog iters sfib (W.fromInt size)
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "parfib" =>
            let
                val cutoff = W.fromInt 19
                val n = Bench.print_bench prog iters (fn i => fib cutoff i) (W.fromInt size)
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "seqbuildfib" =>
            let
                val tr = Bench.print_bench prog iters sbuildfib (W.fromInt size)
                val n = ssumtree tr
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "parbuildfib" =>
            let
                val cutoff = W.fromInt 19
                val tr = Bench.print_bench prog iters (fn i => buildfib cutoff i) (W.fromInt size)
                val n = ssumtree tr
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "seqbuildtree" =>
            let
                val tr = Bench.print_bench prog iters sbuildtree (W.fromInt size)
                val n = ssumtree tr
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "parbuildtree" =>
            let
                val cutoff = W.fromInt 19
                val tr = Bench.print_bench prog iters (fn i => buildtree cutoff i) (W.fromInt size)
                val n = ssumtree tr
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "seqadd1tree" =>
            let
                val tr = sbuildtree (W.fromInt size)
                val tr1 = Bench.print_bench prog iters (fn _ => sadd1tree tr) (W.fromInt size)
                val n = ssumtree tr1
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "paradd1tree" =>
            let
                val tr = sbuildtree (W.fromInt size)
                val cutoff = W.fromInt 19
                val tr1 = Bench.print_bench prog iters (fn _ => add1tree cutoff tr) (W.fromInt size)
                val n = ssumtree tr1
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "seqsumtree" =>
            let
                val tr = sbuildtree (W.fromInt size)
                val n = Bench.print_bench prog iters (fn _ => ssumtree tr) (W.fromInt size)
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | "parsumtree" =>
            let
                val tr = sbuildtree (W.fromInt size)
                val cutoff = W.fromInt 19
                val n = Bench.print_bench prog iters (fn _ => sumtree cutoff tr) (W.fromInt size)
                val _ = print (Int.toString (W.toInt n) ^ "\n")
            in ()
            end
      | _  => raise Fail ("Unknown program: " ^ prog)


val prog = CommandLineArgs.parseString "P" "seqfib"
val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val _ = run prog size iters
