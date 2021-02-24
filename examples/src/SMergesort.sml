structure SMergesort:
sig
  type 'a seq = 'a ArraySlice.slice
  val sortInPlace: ('a * 'a -> order) -> 'a seq -> unit
  val sort: ('a * 'a -> order) -> 'a seq -> 'a seq
end =
struct

  type 'a seq = 'a ArraySlice.slice

  structure AS = ArraySlice
  structure A = Array

  val gotoQuickSort = 8192

  fun take s n = AS.subslice (s, 0, SOME n)
  fun drop s n = AS.subslice (s, n, NONE)

  val allocate = ForkJoin.alloc

  (* in-place sort s, using t as a temporary array if needed *)
  fun sortInPlace' cmp s t =
    if AS.length s <= gotoQuickSort then
      Quicksort.sortInPlace cmp s
    else let
      val half = AS.length s div 2
      val (sl, sr) = (take s half, drop s half)
      val (tl, tr) = (take t half, drop t half)
    in
      (* recursively sort, writing result into t *)
      writeSort cmp sl tl; writeSort cmp sr tr ;
      (* merge back from t into s *)
      SMerge.writeMergeSerial cmp (tl, tr) s;
      ()
    end

  (* destructively sort s, writing the result in t *)
  and writeSort cmp s t =
    if AS.length s <= gotoQuickSort then
      ( Util.for (0, AS.length s) (fn i => AS.update (t, i, (AS.sub(s, i))))
      ; SQuicksort.sortInPlace cmp t
      )
    else let
      val half = AS.length s div 2
      val (sl, sr) = (take s half, drop s half)
      val (tl, tr) = (take t half, drop t half)
    in
      (* recursively in-place sort sl and sr *)
      sortInPlace' cmp sl tl ; sortInPlace' cmp sr tr ;
      (* merge into t *)
      SMerge.writeMergeSerial cmp (sl, sr) t;
      ()
    end

  fun sortInPlace cmp s =
    let
      val t = AS.full (allocate (AS.length s))
    in
      sortInPlace' cmp s t
    end

  fun sort cmp s =
    let
      val result = AS.full (allocate (AS.length s))
    in
      Util.for (0, AS.length s) (fn i => AS.update (result, i, (AS.sub (s, i))));
      sortInPlace cmp result;
      result
    end

end
