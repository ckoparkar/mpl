structure W = Word

fun str_eq s1 s2  =
  case String.compare (s1, s2) of
    EQUAL => true
    | _     => false

fun run prog size iters arr_input =
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
  | "seqbuildkdtree" =>
    let
      val arr = read3DArrayFile arr_input
                                (* val _ = print_arr_point3d arr *)
      val tr = Bench.print_bench prog iters (fn _ => sfromList arr) (W.fromInt size)
      val n = sumkdtree tr
      val _ = print (Real.toString n ^ "\n")
    in ()
    end

  | "parbuildkdtree" =>
    let
      val arr = read3DArrayFile arr_input
      (* val _ = print_arr_point3d arr *)
      (* 2 ^ 19 = 524288 *)
      val cutoff = W.fromInt 524288
      val tr = Bench.print_bench prog iters (fn _ => pfromList cutoff arr) (W.fromInt size)
      val n = sumkdtree tr
      val _ = print (Real.toString n ^ "\n")
    in ()
    end

  | "seqcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      (* val rand = W.toInt (MLtonRandom.rand()) *)
      (* val _ = print (Int.toString rand) *)
      (* val idx = rand mod (AS.length arr) *)
      val idx = 0
      val probe = AS.sub (arr, idx)
      val n = Bench.print_bench prog iters (fn radius => scountCorr' probe radius tr) (W.fromInt size)
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "parcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      (* val rand = W.toInt (gen (W.fromInt 524288) 10) *)
      (* val idx = rand mod (AS.length arr) *)
      val idx = 0
      val probe = AS.sub (arr, idx)
      (* 2 ^ 19 = 524288 *)
      val cutoff = 524288
      val n = Bench.print_bench prog iters (fn radius => pcountCorr' cutoff probe radius tr) (W.fromInt size)
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "seqbhut" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val ps2 = Bench.print_bench prog iters (fn _ => soneStep box mpts ps) (W.fromInt size)
      val err = check ps2
      val _ = print (Real.toString err ^ "\n")
    in
      ()
    end

  | _  => raise Fail ("Unknown program: " ^ prog)


val prog = CommandLineArgs.parseString "P" "seqfib"
val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val arr_input = CommandLineArgs.parseString "A" ""
val _ = run prog size iters arr_input
