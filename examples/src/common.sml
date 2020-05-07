structure A = Array
structure AS = ArraySlice
structure R = Real

(* -------------------------------------------------------------------------- *)

fun str_eq s1 s2  =
  case String.compare (s1, s2) of
    EQUAL => true
    | _     => false

fun str_split (c : char) (s : string) : string list =
  String.fields (fn c' => c = c') s

fun ieq (x : int) (y : int) : bool =
  case Int.compare (x, y) of
    EQUAL => true
    | _ => false

fun compare_real (r1 : R.real) (r2 : R.real) : order =
  if R.< (r1, r2)
  then LESS
  else if R.> (r1, r2)
  then GREATER
  else EQUAL

type point3d = real * real * real

fun compare_point3d (axis : int) ((x1,y1,z1) : point3d) ((x2,y2,z2) : point3d) : order =
  if ieq axis 0
  then compare_real x1 x2
  else if ieq axis 1
  then compare_real y2 y2
  else compare_real z1 z2

fun dist_point3d ((x1,y1,z1) : point3d) ((x2,y2,z2) : point3d) : real =
  let
    val (d1, d2, d3) = (R.-(x1,x2), R.-(y1,y2), R.-(z1,z2))
  in
    R.+(R.*(d1, d1), R.+( R.*(d2, d2), R.*(d3, d3)))
  end

fun enclosing_point3d ((x1,y1,z1) : point3d) ((x2,y2,z2) : point3d) : point3d * point3d =
  let
    val small = (R.min (x1, x2), R.min (y1, y2), R.min (z1, z2))
    val big = (R.max (x1, x2), R.max (y1, y2), R.max (z1, z2))
  in
    (small, big)
  end


fun take s n = AS.subslice (s, 0, SOME n)
fun drop s n = AS.subslice (s, n, NONE)

fun point3dToString ((x,y,z) : point3d) : String.string =
  "(" ^ Real.toString x ^ "," ^ Real.toString y ^ "," ^ Real.toString z ^ ")"

fun print_arr_point3d (arr : point3d AS.slice) : unit =
  ArraySlice.foldl (fn (p,_) => print (point3dToString p ^ "\n")) () arr

fun read3DArrayFile (fp : string) : point3d AS.slice =
  let
    val hdl = TextIO.openIn fp
    val lines = str_split #"\n" (TextIO.inputAll hdl)
    val _   = TextIO.closeIn hdl
    val arr = A.array (List.length lines, (0.0, 0.0, 0.0))
    val _ = List.foldl
              (fn (line, i) =>
                  if (String.size line) = 0
                  then i + 1
                  else
                    let
                      val words = str_split #" " line
                      val a = Option.valOf (Real.fromString (List.nth (words, 0)))
                      val b = Option.valOf (Real.fromString (List.nth (words, 1)))
                      val c = Option.valOf (Real.fromString (List.nth (words, 2)))
                      val pt = (a, b, c)
                      val _ = A.update (arr, i, pt)
                    in
                      i + 1
                    end)
              0
              lines
  in
    AS.slice (arr, 0, SOME (A.length arr - 1))
  end
