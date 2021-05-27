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

fun bench_fn_seq i arr =
  if ieq i 0
  then ()
  else
    let
      val arr2 = SQuicksort.sort (fn (p1, p2) => compare_point3d 0 p1 p2) arr
    in bench_fn_seq (i-1) arr
    end

fun bench_fn_par i arr =
  if ieq i 0
  then ()
  else
    let
      val arr2 = Mergesort.sort (fn (p1, p2) => compare_point3d 0 p1 p2) arr
    in bench_fn_par (i-1) arr
    end

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
      val cutoff = 30
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
      val cutoff = 32000
      val tr = Bench.print_bench prog iters (fn _ => pfromList cutoff arr) size
    in check_buildkdtree arr tr
    end

  | "seqcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val radius = 100.0
      val arr' = AS.subslice(arr, 0, SOME size)
      val counts = Bench.print_bench prog iters (fn _ => allCountCorr_seq radius tr arr') size
      val query = AS.sub(arr', 4)
      val actual = AS.sub(counts, 4)
    in
      ()
    end

  | "parcountcorr" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val idx = 0
      val probe = AS.sub (arr, idx)
      val cutoff = 8000
      val radius = 100.0
      val arr' = AS.subslice(arr, 0, SOME size)
      val counts = Bench.print_bench prog iters (fn _ => allCountCorr_par cutoff radius tr arr') size
      val query = AS.sub(arr', 4)
      val actual = AS.sub(counts, 4)
    in
      ()
    end

  | "parnearest" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val _ = print ("built tree\n")
      val res = Bench.print_bench prog iters (fn _ => allNearestNeighbors_par tr arr) size
    in
     check_nearest arr res
    end

  | "seqnearest" =>
    let
      val arr = read3DArrayFile arr_input
      val tr = sfromList arr
      val res = Bench.print_bench prog iters (fn _ => allNearestNeighbors_seq tr arr) size
    in
     check_nearest arr res
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
      ()
    end

  | "parbhut" =>
    let
      val pts = read2DArrayFile arr_input
      val (box, mpts, ps) = oneStepPre pts
      val bht = sbuildqtree box mpts
      val cutoff = 65536
      val ps2 = Bench.print_bench prog iters (fn _ => poneStep cutoff bht mpts ps) size
    in
      ()
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

  | "seqmkbvh" =>
    let
      val scene = rgbbox
      val bvh = Bench.print_bench prog iters (fn _ =>  mk_bvh_seq sphere_aabb (#spheres scene)) size
    in
      ()
    end

  | "parmkbvh" =>
    let
      val scene = rgbbox
      val bvh = Bench.print_bench prog iters (fn _ =>  mk_bvh_par sphere_aabb (#spheres scene)) size
    in
      ()
    end

  | "seqray" =>
    let
      val scene = rgbbox
      val bvh = mk_bvh_seq sphere_aabb (#spheres scene)
      val cam = camera_from_scene size size scene
      val pixels = Bench.print_bench prog iters (fn size => render_seq bvh size size cam) size
    in
      ()
    end

  | "parray" =>
    let
      val scene = rgbbox
      val bvh = mk_bvh_seq sphere_aabb (#spheres scene)
      val cam = camera_from_scene size size scene
      val pixels = Bench.print_bench prog iters (fn size => render_par bvh size size cam) size
    in
      ()
    end

  | "seqmergesort" =>

    let
      val seed = Random.rand (0, 100)
      val arr = AS.full (A.tabulate (size, (fn i => Random.randReal seed)))
      val sorted = Bench.print_bench prog iters (fn _ => SMergesort.sort Real.compare arr) size
    in
      check_sorted Real.compare sorted
    end
(*
    let
      val seed = Random.rand (0, 100)
      val arr = AS.full (A.tabulate (size, (fn i => let val x = Random.randReal seed
                                                    in (x,x,x)
                                                    end)))
      val sorted = Bench.print_bench prog iters (fn _ => bench_fn_seq 9 arr) size
    in
      ()
    end
*)

  | "parmergesort" =>
    let
      val seed = Random.rand (0, 100)
      val arr = AS.full (A.tabulate (size, (fn i => Random.randReal seed)))
      val sorted = Bench.print_bench prog iters (fn _ => Mergesort.sort Real.compare arr) size
    in
      check_sorted Real.compare sorted
    end
(*
   let
      val seed = Random.rand (0, 100)
      val arr = AS.full (A.tabulate (size, (fn i => let val x = Random.randReal seed
                                                    in (x,x,x)
                                                    end)))
      val sorted = Bench.print_bench prog iters (fn _ => bench_fn_par 9 arr) size
    in
      ()
    end
*)

  | "seqfoldconstants" =>
    let
      val exp = build_exp size
      val exp2 = Bench.print_bench prog iters (fn _ => fold_constants exp) size
    in
      print (Int.toString (sum_exp exp2))
    end

  | "parfoldconstants" =>
    let
      val exp = build_exp size
      val exp2 = Bench.print_bench prog iters (fn _ => fold_constants_par 0 exp) size
    in
      print (Int.toString (sum_exp exp2))
    end

  | "seqcompiler" =>
    let
      val ex = make_big_ex size 0
      val prg = ProgramA (intTy, ex)
      val compiled = Bench.print_bench prog iters (fn _ => compile prg) size
      (* val _ = print_pseudox86 compiled *)
      val _ = print ("\n")
    in ()
    end

  | "parcompiler" =>
    let
      val ex = make_big_ex size 0
      val prg = ProgramA (intTy, ex)
      val compiled = Bench.print_bench prog iters (fn _ => compile_par prg) size
      (* val _ = print_pseudox86 compiled *)
    in ()
    end

  | "seqcompiler2" =>
    let
      val ex = make_big_ex size 0
      val prg = ProgramA (intTy, ex)
      val compiled = Bench.print_bench prog iters (fn _ => compile2 prg) size
      (* val _ = print_pseudox86 compiled *)
      val _ = print ("\n")
    in ()
    end

  | "parcompiler2" =>
    let
      val ex = make_big_ex size 0
      val prg = ProgramA (intTy, ex)
      val compiled = Bench.print_bench prog iters (fn _ => compile2_par prg) size
      (* val _ = print_pseudox86 compiled *)
    in ()
    end


  | _  => raise Fail ("Unknown program: " ^ prog)


val prog = CommandLineArgs.parseString "P" "seqfib"
val size = CommandLineArgs.parseInt "N" 39
val iters = CommandLineArgs.parseInt "I" 9
val arr_input = CommandLineArgs.parseString "A" ""
val _ = run prog size iters arr_input
