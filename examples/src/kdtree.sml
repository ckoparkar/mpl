structure A = Array
structure AS = ArraySlice
structure R = Real
structure I = Int

(* -------------------------------------------------------------------------- *)

datatype kdtree
  = KdEmpty
  | KdLeaf of point3d
  | KdNode of { pivot : point3d
              , num_elems : int
              , split_axis : int
              , min_pt : point3d
              , max_pt : point3d
              , left : kdtree
              , right : kdtree
              }

fun get_min_pt (tr : kdtree) : point3d =
  case tr of
    KdEmpty => (0.0, 0.0, 0.0)
  | KdLeaf pt => pt
  | KdNode{min_pt, ...} => min_pt

fun get_max_pt (tr : kdtree) : point3d =
  case tr of
    KdEmpty => (0.0, 0.0, 0.0)
  | KdLeaf pt => pt
  | KdNode{max_pt, ...} => max_pt

fun get_elems (tr : kdtree) : int =
  case tr of
    KdEmpty => 0
  | KdLeaf pt => 1
  | KdNode{num_elems, ...} => num_elems

fun sfromListWithAxis (axis : int) (pts : point3d AS.slice) : kdtree =
  let
    val len = AS.length pts
  in
    if ieq len 0
    then KdEmpty
    else if ieq len 1
    then KdLeaf (AS.sub (pts, 0))
    else
      let
        val sorted_pts = SQuicksort.sort (fn (p1, p2) => compare_point3d axis p1 p2) pts
        val next_axis = (axis + 1) mod 3
        val pivot_idx = len div 2
        val pivot = AS.sub (sorted_pts, pivot_idx)
        val left_pts = take sorted_pts pivot_idx
        val right_pts = drop sorted_pts (pivot_idx+1)
        val left_tr = sfromListWithAxis next_axis left_pts
        val right_tr = sfromListWithAxis next_axis right_pts
        val small = min_point3d pivot (min_point3d (get_min_pt left_tr) (get_min_pt right_tr))
        val big = max_point3d pivot (max_point3d (get_max_pt left_tr) (get_max_pt right_tr))
        val total_elems = (get_elems left_tr) + (get_elems right_tr) + 1
      in
        KdNode { pivot = pivot
               , num_elems = total_elems
               , split_axis = axis
               , min_pt = small
               , max_pt = big
               , left = left_tr
               , right = right_tr
               }
      end
  end

fun sfromList (pts : point3d AS.slice) : kdtree = sfromListWithAxis 0 pts

fun pfromListWithAxis (cutoff : int) (axis : int) (pts : point3d AS.slice) : kdtree =
  let
    val len = AS.length pts
  in
    if len < cutoff
    then sfromListWithAxis axis pts
    else
      let
        (* parallel sort *)
        val sorted_pts = Mergesort.sort (fn (p1, p2) => compare_point3d axis p1 p2) pts
        (* sequential sort *)
        (* val sorted_pts = SQuicksort.sort (fn (p1, p2) => compare_point3d axis p1 p2) pts *)
        val next_axis = (axis + 1) mod 3
        val pivot_idx = len div 2
        val pivot = AS.sub (sorted_pts, pivot_idx)
        val left_pts = take sorted_pts pivot_idx
        val right_pts = drop sorted_pts (pivot_idx+1)
        val (left_tr, right_tr) =
          ForkJoin.par ( fn _ => pfromListWithAxis cutoff next_axis left_pts,
                         fn _ => pfromListWithAxis cutoff next_axis right_pts)
        val small = min_point3d pivot (min_point3d (get_min_pt left_tr) (get_min_pt right_tr))
        val big = max_point3d pivot (max_point3d (get_max_pt left_tr) (get_max_pt right_tr))
        val total_elems = (get_elems left_tr) + (get_elems right_tr)
      in
        KdNode { pivot = pivot
               , num_elems = total_elems
               , split_axis = axis
               , min_pt = small
               , max_pt = big
               , left = left_tr
               , right = right_tr
               }
      end
  end

fun pfromList (cutoff : int) (pts : point3d AS.slice) : kdtree =
  pfromListWithAxis cutoff 0 pts

fun sumkdtree (tr : kdtree) : real =
  case tr of
    KdEmpty => 0.0
  | KdLeaf (x, y, z) => Real.+ (x, Real.+(y, z))
  | KdNode{pivot,left,right,...} =>
    let
      val (x,y,z) = pivot
    in
      x + y + z + (sumkdtree left) + (sumkdtree right)
    end

fun sumpoints (ls : point3d AS.slice) : real =
  AS.foldl (fn ((x,y,z), acc) => acc + x + y + z) 0.0 ls

fun least_dist (a : point3d) (b : point3d) (c : point3d) : point3d =
  let
    val d1 = dist_point3d a b
    val d2 = dist_point3d a c
  in
    if d1 < d2
    then b
    else c
  end

(* -------------------------------------------------------------------------- *)

fun scountCorr (probe : point3d) (radius : real) (tr : kdtree) : int =
  case tr of
    KdEmpty => 0
  | KdLeaf pt =>
    if (dist_point3d probe pt) < (radius * radius)
    then 1
    else 0
  | KdNode{pivot,num_elems,min_pt,max_pt,left,right,...} =>
    let
      val (min_x,min_y,min_z) = min_pt
      val (max_x,max_y,max_z) = max_pt
      val (p_x,p_y,p_z) = probe
      val center_x = (min_x + max_x) / 2.0
      val center_y = (min_y + max_y) / 2.0
      val center_z = (min_z + max_z) / 2.0
      val d_x = p_x - center_x
      val d_y = p_y - center_y
      val d_z = p_z - center_z
      val boxdist_x = (max_x - min_x) / 2.0
      val boxdist_y = (max_y - min_y) / 2.0
      val boxdist_z = (max_z - min_z) / 2.0
      val sum = (d_x * d_x) + (d_y * d_y) + (d_z * d_z)
      val boxsum = (boxdist_x * boxdist_x) + (boxdist_y * boxdist_y) + (boxdist_z * boxdist_z)
      val (x,y,z) = pivot
    in
      if (sum - boxsum) < (radius * radius)
      then
        let
          val n1 = scountCorr probe radius left
          val n2 = scountCorr probe radius right
        in if (dist_point3d pivot probe) < (radius * radius)
           then n1 + n2 + 1
           else n1 + n2
        end
      else if (dist_point3d pivot probe) < (radius * radius)
           then 1
           else 0
    end

fun pcountCorr (cutoff : int) (probe : point3d) (radius : real) (tr : kdtree) : int =
  case tr of
    KdEmpty => 0
  | KdLeaf pt =>
    if (dist_point3d probe pt) < (radius * radius)
    then 1
    else 0
  | KdNode{pivot,num_elems,min_pt,max_pt,left,right,...} =>
    if num_elems < cutoff then scountCorr probe radius tr else
    let
      val (min_x,min_y,min_z) = min_pt
      val (max_x,max_y,max_z) = max_pt
      val (p_x,p_y,p_z) = probe
      val center_x = (min_x + max_x) / 2.0
      val center_y = (min_y + max_y) / 2.0
      val center_z = (min_z + max_z) / 2.0
      val d_x = p_x - center_x
      val d_y = p_y - center_y
      val d_z = p_z - center_z
      val boxdist_x = (max_x - min_x) / 2.0
      val boxdist_y = (max_y - min_y) / 2.0
      val boxdist_z = (max_z - min_z) / 2.0
      val sum = (d_x * d_x) + (d_y * d_y) + (d_z * d_z)
      val boxsum = (boxdist_x * boxdist_x) + (boxdist_y * boxdist_y) + (boxdist_z * boxdist_z)
    in
      if (sum - boxsum) < (radius * radius)
      then
        let
          val (n1, n2) = ForkJoin.par (fn _ => pcountCorr cutoff probe radius left,
                                       fn _ => pcountCorr cutoff probe radius right)
        in if (dist_point3d pivot probe) < (radius * radius)
           then n1 + n2 + 1
           else n1 + n2
        end
      else if (dist_point3d pivot probe) < (radius * radius)
           then 1
           else 0
    end

(* -------------------------------------------------------------------------- *)

fun nearest (tr : kdtree) (query : point3d) : point3d =
  case tr of
    KdEmpty => (0.0, 0.0, 0.0)
  | KdLeaf pt => pt
  | KdNode{pivot,split_axis,left,right,...} =>
    let
      val tst_query = coord split_axis query
      val tst_pivot = coord split_axis pivot
    in
      if tst_query < tst_pivot
      then find_nearest pivot query tst_pivot tst_query left right
      else find_nearest pivot query tst_pivot tst_query right left
    end

and find_nearest (pivot : point3d) (query : point3d) (tst_pivot : real) (tst_query : real) (good_side : kdtree) (other_side : kdtree) =
  let
    val best0 = nearest good_side query
    val candidate1 = least_dist query best0 pivot
    (* whether the difference between the splitting coordinate of the search point and current node
       is less than the distance (overall coordinates) from the search point to the current best. *)
    val nearest_other_side = tst_query - tst_pivot
  in
    if (nearest_other_side * nearest_other_side) <= (dist_point3d query candidate1)
    then
      let
        val candidate2 = nearest other_side query
        val best1 = least_dist query candidate1 candidate2
      in
        best1
      end
    else
      candidate1
  end

fun allNearestNeighbors_seq (tr : kdtree) (pts : point3d AS.slice) : point3d AS.slice =
  let
    val n = AS.length pts
    val result = A.tabulate (n, (fn i => nearest tr (AS.sub (pts, i))))
  in
    AS.full result
  end

fun allNearestNeighbors_par (tr : kdtree) (pts : point3d AS.slice) : point3d AS.slice =
  let
    val n = AS.length pts
    val result = ForkJoin.alloc n
  in
    ForkJoin.parfor 4096 (0, n)
                    (fn i =>
                        let
                          val j = AS.sub (pts, i)
                          val nn = nearest tr j
                        in
                          A.update (result, i, nn)
                        end);
    AS.full result
  end

fun allCountCorr_seq (radius : real) (tr : kdtree) (pts : point3d AS.slice) : int AS.slice =
  let
    val n = AS.length pts
    val result = A.tabulate (n, (fn i => scountCorr (AS.sub (pts, i)) radius tr))
  in
    AS.full result
  end

fun allCountCorr_par (grain : int) (radius : real) (tr : kdtree) (pts : point3d AS.slice) : int AS.slice =
  let
    val n = AS.length pts
    val result = ForkJoin.alloc n
  in
    ForkJoin.parfor 4 (0, n)
                    (fn i =>
                        let
                          val j = AS.sub (pts, i)
                          val nn = pcountCorr grain (AS.sub (pts, i)) radius tr
                        in
                          A.update (result, i, nn)
                        end);
    AS.full result
  end
