(* -- Ported from : https://smlnj-gitlab.cs.uchicago.edu/manticore/pmlc/blob/15539f76c33fdef2ac7fd5d4396213e06732e660/src/benchmarks/programs/barnes-hut/barnes-hut-seq.pml *)

structure W = Word
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
  = BH_Node of { mp : mass_point
               , total_points : int
               , size: real
               , q1 : bh_tree
               , q2 : bh_tree
               , q3 : bh_tree
               , q4 : bh_tree
               }

  | BH_Leaf of mass_point
  | BH_Empty

val epsilon : real = 0.05
val eClose : real = 0.01
val dt : real = 2.0

fun applyAccel ((PARTICLE (mp, vx, vy)) : particle) ((ax, ay) : point2d) : particle =
  PARTICLE (mp, vx + (ax * dt), vy + (ay * dt))

fun isClose (MP (x1,y1,m)) (p2 : point2d) (size: real) : bool =
  let
    val r2 = (dist_point2d (x1,y1) p2)
    val sizesq = size * size
  in
    r2 < sizesq
  end

fun accel (MP (x1, y1, m1)) (MP (x2, y2, m2)) : (real * real) =
  let
    val dx = x1 - x2
    val dy = y1 - y2
    val rsqr = (dx * dx) + (dy * dy)
    val r = Math.sqrt rsqr
  in
    if r < epsilon
    then (0.0, 0.0)
    else let
      val aabs = m2 / rsqr
    in (aabs * dx, aabs * dy)
    end
  end

fun calcAccel (mpt : mass_point) (bht : bh_tree) : (real * real) =
  case bht of
    BH_Empty => (0.0, 0.0)
  | BH_Leaf (MP (x, y, m)) => accel mpt (MP (x, y, m))
  | BH_Node{mp,total_points,size,q1,q2,q3,q4} =>
    let
      val MP (x,y,m) = mp
    in if isClose mpt (x, y) size
       then
         let
           val ((x1,y1), (x2, y2), (x3, y3), (x4, y4)) =
             ( calcAccel mpt q1
             , calcAccel mpt q2
             , calcAccel mpt q3
             , calcAccel mpt q4
             )
         in
           (x1 + x2 + x3 + x4, y1 + y2 + y3 + y4)
         end
       else
         accel mpt mp
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
  (px > llx) andalso (px <= rux) andalso (py > lly) andalso (py <= ruy)

fun masspointsInBox (box : bounding_box) (mpts : mass_point AS.slice) : mass_point AS.slice =
  AS.full (SeqBasis.filter 8192 (0, AS.length mpts)
                           (fn i => AS.sub (mpts, i))
                           (fn i => inBox box (AS.sub (mpts, i))))

fun total_points (bht : bh_tree) : int =
  case bht of
    BH_Empty => 0
  | BH_Leaf mp => 1
  | BH_Node{total_points,...} => total_points

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
    then BH_Leaf (calcCentroid mpts)
    else
      let
        val mpt = calcCentroid mpts
        val (midx, midy) = ((llx + rux) / 2.0, (lly + ruy) / 2.0)
        val b1 = BOX (llx, lly, midx, midy)
        val b2 = BOX (llx, midy, midx, ruy)
        val b3 = BOX (midx, midy, rux, ruy)
        val b4 = BOX (midx, lly, rux, midy)
        val p1 = masspointsInBox b1 mpts
        val p2 = masspointsInBox b2 mpts
        val p3 = masspointsInBox b3 mpts
        val p4 = masspointsInBox b4 mpts
        val (qq1, qq2, qq3, qq4) =
          ( sbuildqtree b1 p1
          , sbuildqtree b2 p2
          , sbuildqtree b3 p3
          , sbuildqtree b4 p4
          )
        val n = (total_points qq1) + (total_points qq2) + (total_points qq3) + (total_points qq4)
        val size = max_dim box
      in
        BH_Node{mp=mpt,total_points=n,size=size,q1=qq1,q2=qq2,q3=qq3,q4=qq4}
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
    then BH_Leaf (calcCentroid mpts)
    else
      let
        val mpt = calcCentroid mpts
        val (midx, midy) = ((llx + rux) / 2.0, (lly + ruy) / 2.0)
        val b1 = BOX (llx, lly, midx, midy)
        val b2 = BOX (llx, midy, midx, ruy)
        val b3 = BOX (midx, midy, rux, ruy)
        val b4 = BOX (midx, lly, rux, midy)
        val p1 = masspointsInBox b1 mpts
        val p2 = masspointsInBox b2 mpts
        val p3 = masspointsInBox b3 mpts
        val p4 = masspointsInBox b4 mpts
        val (qq1, qq2, qq3, qq4) =
          par4( (fn _ => pbuildqtree cutoff b1 p1)
              , (fn _ => pbuildqtree cutoff b2 p2)
              , (fn _ => pbuildqtree cutoff b3 p3)
              , (fn _ => pbuildqtree cutoff b4 p4)
              )
        val n = (total_points qq1) + (total_points qq2) + (total_points qq3) + (total_points qq4)
        val size = max_dim box
      in
        BH_Node{mp=mpt,total_points=n,size=size,q1=qq1,q2=qq2,q3=qq3,q4=qq4}
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

fun soneStep (box : bounding_box) (mpts : mass_point AS.slice) (ps : particle AS.slice) : particle AS.slice =
  let
    (* val t0 = Time.now() *)
    val bht = sbuildqtree box mpts
    (* val t1 = Time.now() *)
    (* val _ = print("time.tree: " ^ Time.fmt 4 (Time.-(t1, t0)) ^ "\n") *)
    val ps2 = A.tabulate (AS.length ps, (fn i => AS.sub(ps, i)))
    val _ = A.modifyi (fn (i, p) =>
                          let
                            val mpt = AS.sub(mpts, i)
                            val accel = calcAccel mpt bht
                              in
                            applyAccel p accel
                          end)
                      ps2
    (* val t2 = Time.now() *)
    (* val _ = print("time.forces: " ^ Time.fmt 4 (Time.-(t2, t1)) ^ "\n") *)
  in
    AS.full ps2
  end

fun poneStep (cutoff : int) (box : bounding_box) (mpts : mass_point AS.slice) (ps : particle AS.slice) : particle AS.slice =
  let
    (* val t0 = Time.now() *)
    val bht = pbuildqtree cutoff box mpts
    (* val t1 = Time.now() *)
    (* val _ = print("time.tree: " ^ Time.fmt 4 (Time.-(t1, t0)) ^ "\n") *)
    (* parallel loops *)
    val ps2 = AS.full (ForkJoin.alloc (AS.length ps))
    val _ = Util.foreach ps (fn (i,p) =>
                                let
                                  val mpt = AS.sub(mpts, i)
                                  val accel = calcAccel mpt bht
                                  val p2 = applyAccel p accel
                                in
                                  AS.update (ps2, i, p2)
                                end)
    (* val _ = A.foldl (fn (p, _) => print (particle_toString(p) ^ "\n")) () ps2 *)
    (* val t2 = Time.now() *)
    (* val _ = print("time.forces: " ^ Time.fmt 4 (Time.-(t2, t1)) ^ "\n") *)
  in
    ps2
  end

(* -------------------------------------------------------------------------- *)

fun pbbs_length ((x, y) : point2d) : real =
  Math.sqrt ((x * x) + (y * y))

fun check (ps : particle AS.slice) : real =
  let
    val nCheck = 10
    val gGrav = 1.0
    val outer = A.tabulate (nCheck, (fn i => i))
    val err =
      A.foldl
        (fn (i, err) =>
            let
              val idx = if i = 0 then 0 else i-1
              val PARTICLE (MP (_,_,midx), ax_idx, ay_idx) = AS.sub (ps, idx)
              val (force_x, force_y) =
                AS.foldli
                  (fn (j, PARTICLE (MP (_,_,mj), ax_j, ay_j), (force_x, force_y)) =>
                      if idx = j
                      then (force_x, force_y)
                      else
                        let
                          val v = (ax_j - ax_idx, ay_j - ay_idx)
                          val r = pbbs_length v
                          val s = mj * midx * (gGrav / (r * r * r))
                          val (v2_x, v2_y) =
                            (case v of
                               (vx,vy) => (vx*s, vy*s))
                        in
                          (force_x + v2_x, force_y + v2_y)
                        end)
                  (0.0, 0.0)
                  ps
              val force2 = (force_x - ax_idx, force_y - ay_idx)
              val e = pbbs_length(force2) / pbbs_length((force_x, force_y))
              (* val _ = print(Real.toString e ^ "\n") *)
            in
              err + e
            end)
        0.0
        outer
  in
    err / (R.fromInt nCheck)
  end
