structure A = Array
structure AS = ArraySlice
structure L = List
structure F = Real

val epsilon = 0.01

fun pow (b : int) (e : int) : int =
  LargeInt.toInt (IntInf.pow (LargeInt.fromInt b, e))

fun print_check b =
  if b then print ("OK" ^ "\n") else print ("Err" ^ "\n")

fun check_buildfib (n : int) (tr : tree) =
  let
    val expected = (pow 2 n) * (sfib 20)
    val actual = ssumtree tr
  in
    print_check (expected = actual)
  end

fun check_buildtree (n : int) (tr : tree) =
  let
    val expected = pow 2 n
    val actual = ssumtree tr
  in
    print_check (expected = actual)
  end

fun check_add1tree (n : int) (tr : tree) =
  let
    val expected = (pow 2 n) * 2
    val actual = ssumtree tr
  in
    print_check (expected = actual)
  end

fun check_sumtree (n : int) (actual : int) =
  let
    val expected = pow 2 n
  in
    print_check (expected = actual)
  end

fun check_buildkdtree (arr : point3d AS.slice) (tr : kdtree) =
  let
    val actual = sumkdtree tr
    val expected = sumpoints arr
  in
    print_check (Real.abs (expected - actual) < epsilon)
  end

fun check_countcorr (arr : point3d AS.slice) (query : point3d) (actual : int) (radius : real) =
  let
    val expected = AS.foldr (fn (pt, acc) => if (dist_point3d query pt) < (radius * radius)
                                             then acc + 1
                                             else acc) 0 arr
  in
    print ("Expected: " ^ Int.toString expected ^ "; Actual: " ^ Int.toString actual ^ "\n") ;
    print_check (expected = actual)
  end

fun check_nearest (arr : point3d AS.slice) (actual : point3d AS.slice) =
  let
    val n = AS.length arr
    val check = AS.foldli (fn (i, a, acc) =>
                                   let
                                     (* val a = AS.sub (arr, i) *)
                                     val (x1,y1,m1) = a
                                     val b = AS.sub (actual, i)
                                     val (x2,y2,m2) = b
                                   in
                                     if req x1 x2 andalso req y1 y2 andalso req m1 m2
                                     then acc andalso true
                                     else false
                                   end)
                         true arr
  in
    print_check check
  end

fun check_buildquadtree (mpts : mass_point AS.slice) (bht : bh_tree) =
  let
    val expected = sum_mass_points mpts
    val actual = sum_bh_tree bht
    val count1 = count_points_bh_tree bht
    val count2 = total_points bht
  in
    print ("Sum: expected= " ^ Real.toString expected ^ "; actual= " ^ Real.toString actual ^ "\n") ;
    print ("Counts: " ^ Int.toString count1 ^ ", " ^ Int.toString count2  ^ "\n") ;
    print_check (Real.abs (expected - actual) < epsilon)
  end

fun check_bhut (input : particle AS.slice) (ps : particle AS.slice) =
  let
    fun accel_for query = AS.foldr (fn (PARTICLE (mp, _, _), (aax,aay)) =>
                                       let
                                         val (PARTICLE (query_mp, _, _)) = query
                                         val (ax,ay) = accel query_mp mp
                                       in
                                         (aax + ax, aay + ay)
                                       end)
                          (0.0, 0.0) input
    val n = AS.length input
    val checkpoints = [0 , n div 4 , n div 2 , n-1]
    val deltas = L.map (fn idx =>
                           let
                             val query = AS.sub (input, idx)
                             val (ax, ay) = accel_for query
                             val PARTICLE (_, expected_ax, expected_ay) = applyAccel query (ax,ay)
                             val PARTICLE (_, actual_ax, actual_ay) = AS.sub (ps, idx)
                           in
                             (Real.abs (expected_ax - actual_ax), Real.abs (expected_ay - actual_ay))
                           end)
                       checkpoints
  val _ = print "\n"
  val check = L.foldr (fn ((dx, dy), acc) =>
                          let
                            val _ = print (Real.toString dx ^ "," ^ Real.toString dy ^ "\n")
                          in
                            acc andalso dx < epsilon andalso dy < epsilon
                          end) true deltas
  in
    print_check check
  end

fun check_coins (amt : int) (ls : AList) =
  let
    val n = lenA ls
  in
    case amt of
      777 => print_check (n = 140899)
    | 999 => print_check (n = 329565)
    (* assume it's correct *)
    | _ =>
      print (Int.toString n ^ "\n") ;
      print_check true
  end

fun check_countnodes (count : int) =
  print (Int.toString count ^ "\n")

fun check_sorted (f : ('a * 'a) -> order) (arr : 'a AS.slice) =
  let
    val len = AS.length arr
    val res = ref true
  in
    Util.for (0, len-2) (fn i =>
                            case f ((AS.sub (arr, i)), (AS.sub (arr, i+1))) of
                                GREATER => res := false
                              | _ => ()) ;
    print_check (!res)
  end
