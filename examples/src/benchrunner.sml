structure P = SExpParser
structure L = List

fun get_rand (n : int) : int =
  let
    val t = Time.now()
    val i = Real.round (Time.toReal t)
  in
    Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), Word64.fromInt n))
  end

fun str_eq s1 s2  =
  case String.compare (s1, s2) of
    EQUAL => true
    | _     => false

fun run prog size iters arr_input =
  case prog of
    "seqfib" =>
    let
      val n = Bench.print_bench prog iters sfib size
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "parfib" =>
    let
      val cutoff = 19
      val n = Bench.print_bench prog iters (fn i => fib cutoff i) size
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "seqbuildfib" =>
    let
      val tr = Bench.print_bench prog iters sbuildfib size
      val n = ssumtree tr
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "parbuildfib" =>
    let
      val cutoff = 8
      val tr = Bench.print_bench prog iters (fn i => buildfib cutoff i) size
      val n = ssumtree tr
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "seqbuildtree" =>
    let
      val tr = Bench.print_bench prog iters sbuildtree size
      val n = ssumtree tr
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "parbuildtree" =>
    let
      val cutoff = 19
      val tr = Bench.print_bench prog iters (fn i => buildtree cutoff i) size
      val n = ssumtree tr
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "seqadd1tree" =>
    let
      val tr = sbuildtree size
      val tr1 = Bench.print_bench prog iters (fn _ => sadd1tree tr) size
      val n = ssumtree tr1
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "paradd1tree" =>
    let
      val tr = sbuildtree size
      val cutoff = 19
      val tr1 = Bench.print_bench prog iters (fn _ => add1tree cutoff tr) size
      val n = ssumtree tr1
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "seqsumtree" =>
    let
      val tr = sbuildtree size
      val n = Bench.print_bench prog iters (fn _ => ssumtree tr) size
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "parsumtree" =>
    let
      val tr = sbuildtree size
      val cutoff = 19
      val n = Bench.print_bench prog iters (fn _ => sumtree cutoff tr) size
      val _ = print (Int.toString n ^ "\n")
    in ()
    end
  | "seqbuildkdtree" =>
    let
      val arr = read3DArrayFile arr_input
      (* val _ = print_arr_point3d arr *)
      val tr = Bench.print_bench prog iters (fn _ => sfromList arr) size
      val n = sumkdtree tr
      val _ = print (Real.toString n ^ "\n")
    in ()
    end

  | "parbuildkdtree" =>
    let
      val arr = read3DArrayFile arr_input
      (* val _ = print_arr_point3d arr *)
      (* 2 ^ 19 = 524288 *)
      val cutoff = 524288
      val tr = Bench.print_bench prog iters (fn _ => pfromList cutoff arr) size
      val n = sumkdtree tr
      val _ = print (Real.toString n ^ "\n")
    in ()
    end

  | "seqcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val n = Bench.print_bench prog iters (fn radius =>
                                               let
                                                 val rand = get_rand(AS.length arr)
                                                 val probe = AS.sub (arr, rand)
                                               in
                                                 scountCorr probe (Real.fromInt radius) tr
                                               end)
                                           size
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "parcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val idx = 0
      val probe = AS.sub (arr, idx)
      (* 2 ^ 19 = 524288 *)
      val cutoff = 524288
      val n = Bench.print_bench prog iters (fn radius =>
                                               let
                                                 val rand = get_rand(AS.length arr)
                                                 val probe = AS.sub (arr, rand)
                                               in
                                                 pcountCorr cutoff probe (Real.fromInt radius) tr
                                               end)
                                           size
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "seqbuildquadtree" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = Bench.print_bench prog iters (fn _ => sbuildqtree box mpts) size
      val sum = sum_bh_tree bht
      val _ = print (Real.toString sum ^ "\n")
    in
      ()
    end

  | "parbuildquadtree" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val cutoff = 65536
      val bht = Bench.print_bench prog iters (fn _ => pbuildqtree cutoff box mpts) size
      val sum = sum_bh_tree bht
      val _ = print (Real.toString sum ^ "\n")
    in
      ()
    end

  | "seqbhut" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = sbuildqtree box mpts
      val ps2 = Bench.print_bench prog iters (fn _ => soneStep bht mpts ps) size
      val err = check ps2
      val _ = print (Real.toString err ^ "\n")
    in
      ()
    end

  | "parbhut" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = sbuildqtree box mpts
      val cutoff = 65536
      val ps2 = Bench.print_bench prog iters (fn _ => poneStep cutoff bht mpts ps) size
      val err = check ps2
      val _ = print (Real.toString err ^ "\n")
    in
      ()
    end

  | "seqcoins" =>
    let
      val tr = Bench.print_bench prog iters (fn amt => payA_seq amt coins_input) size
      val n = lenA tr
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "parcoins" =>
    let
      val tr = Bench.print_bench prog iters (fn amt => payA_par 3 amt coins_input) size
      val n = lenA tr
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "seqcountnodes" =>
    let
      val bench_fn = (fn _ =>
                         let
                           val s = P.parseFile arr_input
                           val e = parse_toplvl (L.hd s)
                         in
                           countnodes e
                         end)
      val n = Bench.print_bench prog iters bench_fn size
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "parcountnodes" =>
    let
      val bench_fn = (fn _ =>
                         let
                           val s = P.parseFile arr_input
                           val e = parse_toplvl (L.hd s)
                         in
                           par_countnodes e
                         end)
      val n = Bench.print_bench prog iters bench_fn size
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "seqcountnodes2" =>
    let
      val s = P.parseFile arr_input
      val e = parse_toplvl (L.hd s)
      val n = Bench.print_bench prog iters (fn _ => countnodes e) size
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | "parcountnodes2" =>
    let
      val s = P.parseFile arr_input
      val e = parse_toplvl (L.hd s)
      val n = Bench.print_bench prog iters (fn _ => par_countnodes e) size
      val _ = print (Int.toString n ^ "\n")
    in
      ()
    end

  | _  => raise Fail ("Unknown program: " ^ prog)


val prog = CommandLineArgs.parseString "P" "seqfib"
val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val arr_input = CommandLineArgs.parseString "A" ""
val _ = run prog size iters arr_input
