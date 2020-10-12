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
    in check_buildfib size tr
    end

  | "parbuildfib" =>
    let
      val cutoff = 6
      val tr = Bench.print_bench prog iters (fn i => buildfib cutoff i) size
    in check_buildfib size tr
    end

  | "seqbuildtree" =>
    let
      val tr = Bench.print_bench prog iters sbuildtree size
    in check_buildtree size tr
    end

  | "parbuildtree" =>
    let
      val cutoff = 19
      val tr = Bench.print_bench prog iters (fn i => buildtree cutoff i) size
    in check_buildtree size tr
    end

  | "seqadd1tree" =>
    let
      val tr = sbuildtree size
      val tr1 = Bench.print_bench prog iters (fn _ => sadd1tree tr) size
    in check_add1tree size tr1
    end

  | "paradd1tree" =>
    let
      val tr = sbuildtree size
      val cutoff = 19
      val tr1 = Bench.print_bench prog iters (fn _ => add1tree cutoff tr) size
    in check_add1tree size tr1
    end

  | "seqsumtree" =>
    let
      val tr = sbuildtree size
      val n = Bench.print_bench prog iters (fn _ => ssumtree tr) size
    in check_sumtree size n
    end

  | "parsumtree" =>
    let
      val tr = sbuildtree size
      val cutoff = 19
      val n = Bench.print_bench prog iters (fn _ => sumtree cutoff tr) size
    in check_sumtree size n
    end

  | "seqbuildkdtree" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = Bench.print_bench prog iters (fn _ => sfromList arr) size
    in check_buildkdtree arr tr
    end

  | "parbuildkdtree" =>
    let
      val arr = read3DArrayFile arr_input
      (* val _ = print_arr_point3d arr *)
      (* 2 ^ 19 = 524288 *)
      val cutoff = 524288
      val tr = Bench.print_bench prog iters (fn _ => pfromList cutoff arr) size
    in check_buildkdtree arr tr
    end

  | "seqcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      (* val (query, actual) = Bench.print_bench prog iters (fn radius =>
                                               let
                                                 val rand = get_rand(AS.length arr)
                                                 val probe = AS.sub (arr, rand)
                                                 val corr = scountCorr probe (Real.fromInt radius) tr
                                               in
                                                 (probe, corr)
                                               end)
                                           size *)
      val radius = 10.0
      val arr' = AS.subslice(arr, 0, SOME size)
      val counts = Bench.print_bench prog iters (fn _ => allCountCorr_seq radius tr arr') size
      val query = AS.sub(arr', 4)
      val count = AS.sub(counts, 4)
    in
      check_countcorr arr query count radius
    end

  | "parcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val idx = 0
      val probe = AS.sub (arr, idx)
      (* 2 ^ 19 = 524288 *)
      val cutoff = 50000
      (* val (query, actual) = Bench.print_bench prog iters (fn radius =>
                                               let
                                                 val rand = get_rand(AS.length arr)
                                                 val probe = AS.sub (arr, rand)
                                                 val corr = pcountCorr cutoff probe (Real.fromInt radius) tr
                                               in
                                                 (probe, corr)
                                               end)
                                           size *)
      val radius = 10.0
      val arr' = AS.subslice(arr, 0, SOME size)
      val counts = Bench.print_bench prog iters (fn _ => allCountCorr_par cutoff radius tr arr') size
      val query = AS.sub(arr', 4)
      val count = AS.sub(counts, 4)
    in
      check_countcorr arr query count (Real.fromInt size)
    end

  | "parnearest" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val _ = print ("built tree\n")
      val cutoff = 1024
      val res = Bench.print_bench prog iters (fn _ => allNearestNeighbors_par cutoff tr arr) size
    in
     ()
    end

  | "seqnearest" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val res = Bench.print_bench prog iters (fn _ => allNearestNeighbors_seq tr arr) size
    in
     ()
    end

  | "seqbuildquadtree" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = Bench.print_bench prog iters (fn _ => sbuildqtree box mpts) size
      val total_points = count_points_bh_tree bht
    in
      check_buildquadtree mpts bht
    end

  | "parbuildquadtree" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val cutoff = 65536
      val bht = Bench.print_bench prog iters (fn _ => pbuildqtree cutoff box mpts) size
    in
      check_buildquadtree mpts bht
    end

  | "seqbhut" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = sbuildqtree box mpts
      val ps2 = Bench.print_bench prog iters (fn _ => soneStep bht mpts ps) size
    in
      check_bhut ps ps2
    end

  | "parbhut" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = sbuildqtree box mpts
      val cutoff = 65536
      val ps2 = Bench.print_bench prog iters (fn _ => poneStep cutoff bht mpts ps) size
    in
      check_bhut ps ps2
    end

  | "seqcoins" =>
    let
      val tr = Bench.print_bench prog iters (fn amt => payA_seq amt coins_input) size
    in
      check_coins size tr
    end

  | "parcoins" =>
    let
      val tr = Bench.print_bench prog iters (fn amt => payA_par 3 amt coins_input) size
    in
      check_coins size tr
    end

  | "seqcountnodes" =>
    let
      val t0 = Time.now()
      val s = P.parseFile arr_input
      val e = parse_toplvl (L.hd s)
      val t1 = Time.now()
      val _ = print ("Parsed in: " ^ Time.fmt 4 (Time.-(t1, t0)) ^ "\n")
      val n = Bench.print_bench prog iters (fn _ => countnodes e) size
    in
      check_countnodes n
    end

  | "parcountnodes" =>
    let
      val t0 = Time.now()
      val s = P.parseFile arr_input
      val e = parse_toplvl (L.hd s)
      val t1 = Time.now()
      val _ = print ("Parsed in: " ^ Time.fmt 4 (Time.-(t1, t0)) ^ "\n")
      val n = Bench.print_bench prog iters (fn _ => par_countnodes e) size
    in
      check_countnodes n
    end

  | "seqcountnodes2" =>
    let
      val bench_fn = (fn _ =>
                         let
                           val s = P.parseFile arr_input
                           val e = parse_toplvl (L.hd s)
                         in
                           countnodes e
                         end)
      val n = Bench.print_bench prog iters bench_fn size
    in
      check_countnodes n
    end

  | "parcountnodes2" =>
    let
      val bench_fn = (fn _ =>
                         let
                           val s = P.parseFile arr_input
                           val e = parse_toplvl (L.hd s)
                         in
                           countnodes e
                         end)
      val n = Bench.print_bench prog iters bench_fn size
    in
      check_countnodes n
    end

  | _  => raise Fail ("Unknown program: " ^ prog)


val prog = CommandLineArgs.parseString "P" "seqfib"
val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val arr_input = CommandLineArgs.parseString "A" ""
val _ = run prog size iters arr_input
