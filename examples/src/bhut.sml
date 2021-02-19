(* -- Ported from : https://smlnj-gitlab.cs.uchicago.edu/manticore/pmlc/blob/15539f76c33fdef2ac7fd5d4396213e06732e660/src/benchmarks/programs/barnes-hut/barnes-hut-seq.pml *)

structure A = Array
structure AS = ArraySlice
structure R = Real

(* -------------------------------------------------------------------------- *)

type point2d = (real * real)

datatype bounding_box = BOX of (real * real * real * real)

fun bounding_box_toString (BOX (a, b, c, d)) =
  "(" ^ Real.toString a ^ "," ^ Real.toString b ^ "," ^ Real.toString c ^ "," ^ Real.toString d ^ ")"

datatype mass_point = MP of (real * real * real)

fun mass_point_toString (MP (a, b, c)) =
  "(" ^ Real.toString a ^ "," ^ Real.toString b ^ "," ^ Real.toString c ^ "," ^  ")"

datatype particle = PARTICLE of (mass_point * real * real)

fun particle_toString (PARTICLE (a, b, c)) =
  "(" ^ mass_point_toString a ^ "," ^ Real.toString b ^ "," ^ Real.toString c ^ "," ^  ")"

datatype bh_tree
  = BH_Node of { centroid : mass_point
               , total_points : int
               , width: real
               , tr1 : bh_tree
               , tr2 : bh_tree
               , tr3 : bh_tree
               , tr4 : bh_tree
               }
  | BH_Leaf of mass_point
  | BH_Empty

fun sum_bh_tree bht =
  case bht of
    BH_Empty => 0.0
  | BH_Leaf (MP (x,y,m)) => x + y + m
  | BH_Node{centroid,total_points,width,tr1,tr2,tr3,tr4} =>
    (sum_bh_tree tr1) + (sum_bh_tree tr2) + (sum_bh_tree tr3) + (sum_bh_tree tr4)

fun count_points_bh_tree bht =
  case bht of
    BH_Empty => 0
  | BH_Leaf (MP (x,y,m)) => 1
  | BH_Node{tr1,tr2,tr3,tr4,...} =>
    (count_points_bh_tree tr1) + (count_points_bh_tree tr2) + (count_points_bh_tree tr3) + (count_points_bh_tree tr4)

fun sum_mass_points (ls : mass_point AS.slice) : real =
  AS.foldl (fn ((MP (x,y,z)), acc) => acc + x + y + z) 0.0 ls

val dt : real = 2.0

fun applyAccel ((PARTICLE (mp, vx, vy)) : particle) ((ax, ay) : point2d) : particle =
  PARTICLE (mp, vx + (ax * dt), vy + (ay * dt))

fun isClose (MP (x1,y1,m)) (p2 : point2d) (width: real) : bool =
  let
    val r2 = (dist_point2d (x1,y1) p2)
    val widthsq = width * width
  in
    r2 < widthsq
    (* (width / r2) < 0.5 *)
    (* print (Real.toString width ^ "\n") ; *)
    (* (r2 < 2.0) *)
  end

fun accel (MP (x1, y1, m1)) (MP (x2, y2, m2)) : (real * real) =
  if Real.==(x1,x2) andalso Real.==(y1,y2) andalso Real.==(m1,m2)
  then (0.0, 0.0)
  else
    let
      val dx = x1 - x2
      val dy = y1 - y2
      val rsqr = (dx * dx) + (dy * dy)
      val r = Math.sqrt rsqr
      val s = (m1 * m2 / (rsqr * r))
    in
      (dx * s, dy * s)
    end

fun calcAccel (mpt : mass_point) (bht : bh_tree) : (real * real) =
  case bht of
    BH_Empty => (0.0, 0.0)
  | BH_Leaf (MP (x, y, m)) => accel mpt (MP (x, y, m))
  | BH_Node{centroid,total_points,width,tr1,tr2,tr3,tr4} =>
    let val MP (x,y,_) = centroid
    in
       if
        isClose mpt (x,y) width
        (* true *)
       then
         let
           val ((x1,y1), (x2, y2), (x3, y3), (x4, y4)) =
             ( calcAccel mpt tr1
             , calcAccel mpt tr2
             , calcAccel mpt tr3
             , calcAccel mpt tr4
             )
         in
           (x1 + x2 + x3 + x4, y1 + y2 + y3 + y4)
         end
       else
         accel mpt centroid
    end


fun calcCentroid (mpts : mass_point AS.slice) : mass_point =
  let
    val (sum_mx, sum_my, sum_m) =
      AS.foldl (fn (MP(x, y, m), (acc_x,acc_y,acc_m)) =>
                   (acc_x + (x*m), acc_y + (y*m), acc_m + m))
               (0.0, 0.0, 0.0)
               mpts
  in
    MP (sum_mx / sum_m, sum_my / sum_m, sum_m)
  end

fun inBox (BOX (llx, lly, rux, ruy)) (MP (px, py, mx)) =
  (px >= llx) andalso (px <= rux) andalso (py >= lly) andalso (py <= ruy)

fun masspointsInBox (box : bounding_box) (mpts : mass_point AS.slice) : mass_point AS.slice =
  AS.full (SeqBasis.filter 8192 (0, AS.length mpts)
                           (fn i => AS.sub (mpts, i))
                           (fn i => inBox box (AS.sub (mpts, i))))

fun total_points (bht : bh_tree) : int =
  case bht of
    BH_Empty => 0
  | BH_Leaf mp => 1
  | BH_Node{total_points,...} => total_points

fun centroid (bht : bh_tree) : mass_point =
  case bht of
    BH_Empty => MP (0.0, 0.0, 1.0)
  | BH_Leaf mp => mp
  | BH_Node{centroid,...}  => centroid

fun max_dim (BOX (llx,lly,rux,ruy) : bounding_box) : real =
  Real.max((rux-llx), (ruy-lly))

fun sbuildqtree (box : bounding_box) (mpts : mass_point AS.slice) : bh_tree =
  let
    val len = AS.length mpts
    val BOX (llx, lly, rux, ruy) = box
  in
    if len = 0
    then BH_Empty
    else if len = 1
    then BH_Leaf (AS.sub (mpts, 0))
    else
      let
        val (midx, midy) = ((llx + rux) / 2.0, (lly + ruy) / 2.0)
        val b1 = BOX (llx, lly, midx, midy)
        val b2 = BOX (llx, midy, midx, ruy)
        val b3 = BOX (midx, midy, rux, ruy)
        val b4 = BOX (midx, lly, rux, midy)
        val p1 = masspointsInBox b1 mpts
        val p2 = masspointsInBox b2 mpts
        val p3 = masspointsInBox b3 mpts
        val p4 = masspointsInBox b4 mpts
        val (tr1, tr2, tr3, tr4) =
          ( sbuildqtree b1 p1
          , sbuildqtree b2 p2
          , sbuildqtree b3 p3
          , sbuildqtree b4 p4
          )
        val n = (total_points tr1) + (total_points tr2) + (total_points tr3) + (total_points tr4)
        val centroid = calcCentroid mpts
        (* val centroid = *)
        (*   let *)
        (*     val (MP (x1,y1,m1), n1) = (centroid tr1, total_points tr1) *)
        (*     val (MP (x2,y2,m2), n2) = (centroid tr2, total_points tr2) *)
        (*     val (MP (x3,y3,m3), n3) = (centroid tr3, total_points tr3) *)
        (*     val (MP (x4,y4,m4), n4) = (centroid tr4, total_points tr4) *)
        (*     val x = Real./(Real.+(Real.+(Real.*((Real.fromInt n1), x1), Real.*((Real.fromInt n2), x2)), Real.+(Real.*((Real.fromInt n3), x3), Real.*((Real.fromInt n4), x4))), (Real.fromInt n)) *)
        (*     val y = Real./(Real.+(Real.+(Real.*((Real.fromInt n1), y1), Real.*((Real.fromInt n2), y2)), Real.+(Real.*((Real.fromInt n3), y3), Real.*((Real.fromInt n4), y4))), (Real.fromInt n)) *)
        (*     val m = Real./(Real.+(Real.+(Real.*((Real.fromInt n1), m1), Real.*((Real.fromInt n2), m2)), Real.+(Real.*((Real.fromInt n3), m3), Real.*((Real.fromInt n4), m4))), (Real.fromInt n)) *)
        (*   in *)
        (*     MP (x,y,m) *)
        (*   end *)
        val width = max_dim box
      in
        BH_Node{centroid=centroid,total_points=n,width=width,tr1=tr1,tr2=tr2,tr3=tr3,tr4=tr4}
      end
  end

fun par4 (a, b, c, d) =
    let
      val ((ar, br), (cr, dr)) =
        ForkJoin.par (fn _ => ForkJoin.par (a, b),
                      fn _ => ForkJoin.par (c, d))
    in
      (ar, br, cr, dr)
    end


fun pbuildqtree (cutoff : int) (box : bounding_box) (mpts : mass_point AS.slice) : bh_tree =
  let
    val len = AS.length mpts
    val BOX (llx, lly, rux, ruy) = box
  in
    if len < cutoff
    then sbuildqtree box mpts
    else if len = 0
    then BH_Empty
    else if len = 1
    then BH_Leaf (AS.sub (mpts, 0))
    else
      let
        val (midx, midy) = ((llx + rux) / 2.0, (lly + ruy) / 2.0)
        val b1 = BOX (llx, lly, midx, midy)
        val b2 = BOX (llx, midy, midx, ruy)
        val b3 = BOX (midx, midy, rux, ruy)
        val b4 = BOX (midx, lly, rux, midy)
        val p1 = masspointsInBox b1 mpts
        val p2 = masspointsInBox b2 mpts
        val p3 = masspointsInBox b3 mpts
        val p4 = masspointsInBox b4 mpts
        val (qtr1, qtr2, qtr3, qtr4) =
          par4( (fn _ => pbuildqtree cutoff b1 p1)
              , (fn _ => pbuildqtree cutoff b2 p2)
              , (fn _ => pbuildqtree cutoff b3 p3)
              , (fn _ => pbuildqtree cutoff b4 p4)
              )
        val n = (total_points qtr1) + (total_points qtr2) + (total_points qtr3) + (total_points qtr4)
        val centroid = calcCentroid mpts
        val width = max_dim box
      in
        BH_Node{centroid=centroid,total_points=n,width=width,tr1=qtr1,tr2=qtr2,tr3=qtr3,tr4=qtr4}
      end
  end


(* -------------------------------------------------------------------------- *)

fun oneStepPre (pts : point2d AS.slice) : (bounding_box * mass_point AS.slice * particle AS.slice) =
  let
    val len = AS.length pts
    val llx = AS.foldl (fn ((x,y), acc) => R.min (acc, x)) R.maxFinite pts
    val lly = AS.foldl (fn ((x,y), acc) => R.min (acc, y)) R.maxFinite pts
    val rux = AS.foldl (fn ((x,y), acc) => R.max (acc, x)) (~1.0 * R.maxFinite) pts
    val ruy = AS.foldl (fn ((x,y), acc) => R.max (acc, y)) (~1.0 * R.maxFinite) pts
    val mpts = A.tabulate (len, (fn i => let val (x,y) = AS.sub(pts, i) in MP(x,y,1.0) end))
    val ps = A.tabulate (len, (fn i => let val (x,y) = AS.sub(pts, i) in PARTICLE(MP(x,y,1.0), 0.0, 0.0) end))
  in
    (BOX (llx, lly, rux, ruy), AS.full mpts, AS.full ps)
  end

fun soneStep (bht : bh_tree) (mpts : mass_point AS.slice) (ps : particle AS.slice) : particle AS.slice =
  let
    val ps2 = A.tabulate (AS.length ps, (fn i => AS.sub(ps, i)))
    val _ = A.modifyi (fn (i, p) =>
                          let
                            val mpt = AS.sub(mpts, i)
                            val accel = calcAccel mpt bht
                              in
                            applyAccel p accel
                          end)
                      ps2
  in
    AS.full ps2
  end

fun poneStep (cutoff : int) (bht : bh_tree) (mpts : mass_point AS.slice) (ps : particle AS.slice) : particle AS.slice =
  let
    val ps2 = AS.full (ForkJoin.alloc (AS.length ps))
    val f = (fn (i,p) =>
                                let
                                  val mpt = AS.sub(mpts, i)
                                  val accel = calcAccel mpt bht
                                  val p2 = applyAccel p accel
                                in
                                  AS.update (ps2, i, p2)
                                end)
    val _ = ForkJoin.parfor 4096
                            (0, AS.length ps)
                            (fn i => f (i, AS.sub (ps, i)))
    (* val _ = Util.foreach ps fn *)
  in
    ps2
  end
