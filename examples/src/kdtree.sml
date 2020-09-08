structure A = Array
structure AS = ArraySlice
structure R = Real
structure I = Int

(* -------------------------------------------------------------------------- *)

datatype kdtree
  = KdLeaf of point3d
  | KdNode of { num_elems : int
              , split_axis : int
              , min_pt : point3d
              , max_pt : point3d
              , left : kdtree
              , right : kdtree
              }

fun get_min_pt (tr : kdtree) : point3d =
  case tr of
    KdLeaf pt => pt
  | KdNode{min_pt, ...} => min_pt

fun get_max_pt (tr : kdtree) : point3d =
  case tr of
    KdLeaf pt => pt
  | KdNode{max_pt, ...} => max_pt

fun get_elems (tr : kdtree) : int =
  case tr of
    KdLeaf pt => 1
  | KdNode{num_elems, ...} => num_elems

fun sfromListWithAxis (axis : int) (pts : point3d AS.slice) : kdtree =
  let
    val len = AS.length pts
  in
    if ieq len 1
    then KdLeaf (AS.sub (pts, 0))
    else
      let
        val sorted_pts = SQuicksort.sort (fn (p1, p2) => compare_point3d axis p1 p2) pts
        val next_axis = (axis + 1) mod 3
        val pivot_idx = len div 2
        val pivot = AS.sub (sorted_pts, pivot_idx)
        val left_pts = take sorted_pts pivot_idx
        val right_pts = drop sorted_pts pivot_idx
        val left_tr = sfromListWithAxis next_axis left_pts
        val right_tr = sfromListWithAxis next_axis right_pts
        val (small, big) = enclosing_point3d (get_min_pt left_tr) (get_max_pt left_tr)
        val total_elems = (get_elems left_tr) + (get_elems right_tr)
      in
        KdNode { num_elems = total_elems
               , split_axis = next_axis
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
        (* val sorted_pts = Mergesort.sort (fn (p1, p2) => compare_point3d axis p1 p2) pts *)
        (* sequential sort *)
        val sorted_pts = SQuicksort.sort (fn (p1, p2) => compare_point3d axis p1 p2) pts
        val next_axis = (axis + 1) mod 3
        val pivot_idx = len div 2
        val pivot = AS.sub (sorted_pts, pivot_idx)
        val left_pts = take sorted_pts pivot_idx
        val right_pts = drop sorted_pts pivot_idx
        val (left_tr, right_tr) =
          ForkJoin.par ( fn _ => pfromListWithAxis cutoff next_axis left_pts,
                         fn _ => pfromListWithAxis cutoff next_axis right_pts)
        val (small, big) = enclosing_point3d (get_min_pt left_tr) (get_max_pt left_tr)
        val total_elems = (get_elems left_tr) + (get_elems right_tr)
      in
        KdNode { num_elems = total_elems
               , split_axis = next_axis
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
    KdLeaf (x, y, z) => Real.+ (x, Real.+(y, z))
  | KdNode{left,right,...} => Real.+ (sumkdtree left, sumkdtree right)

(* -------------------------------------------------------------------------- *)

fun scountCorr (probe : point3d) (radius : real) (tr : kdtree) : int =
  case tr of
    KdLeaf pt =>
    if (dist_point3d probe pt) < (radius * radius)
    then 1
    else 0
  | KdNode{num_elems,min_pt,max_pt,left,right,...} =>
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
          val n1 = scountCorr probe radius left
          val n2 = scountCorr probe radius right
        in n1 + n2
        end
      else 0
    end

fun pcountCorr (cutoff : int) (probe : point3d) (radius : real) (tr : kdtree) : int =
  case tr of
    KdLeaf pt =>
    if (dist_point3d probe pt) < (radius * radius)
    then 1
    else 0
  | KdNode{num_elems,min_pt,max_pt,left,right,...} =>
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
          val (n1, n2) = ForkJoin.par ( fn _ => pcountCorr cutoff probe radius left
                                      , fn _ => pcountCorr cutoff probe radius right)
        in n1 + n2
        end
      else 0
    end
