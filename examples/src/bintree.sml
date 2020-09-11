datatype tree = Leaf of int
              | Node of int * tree * tree;

(* build tree *)
fun sbuildtree n =
  if n <= 0
  then Leaf 1
  else
    let val (x, y) = (sbuildtree (n-1),
                      sbuildtree (n-1))
    in Node (n, x, y)
    end

fun buildtree cutoff n =
  if n <= cutoff
  then sbuildtree n
  else
    let val (x, y) = ForkJoin.par ((fn _ => buildtree cutoff (n-1)),
                                   (fn _ => buildtree cutoff (n-1)))
    in Node (n, x, y)
    end

(* add1 tree *)
fun sadd1tree tr =
  case tr of
    Leaf (i)       => Leaf (i+1)
  | Node (n, x, y) =>
    let
      val(a,b) = (sadd1tree x, sadd1tree y)
    in
      Node(n, a, b)
    end

fun add1tree cutoff tr =
  case tr of
    Leaf (i)       => Leaf (i+1)
  | Node (n, x, y) =>
      if n < cutoff
      then sadd1tree tr
      else
        let
          val (a,b) =  ForkJoin.par((fn _ => add1tree cutoff x), (fn _ => add1tree cutoff y))
        in
          Node(n, a, b)
        end

(* sum tree *)
fun ssumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => (ssumtree x) + (ssumtree y)

fun sumtree cutoff tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) =>
    if n < cutoff
    then ssumtree tr
    else
      let
        val (i,j) = ForkJoin.par((fn _ => sumtree cutoff x), (fn _ => sumtree cutoff y))
      in
        i + j
      end


(* build fib *)
fun sbuildfib n =
  if n <= 0
  then Leaf (sfib 20)
  else
    let val (x, y) = (sbuildfib (n-1), sbuildfib (n-1))
    in Node (n, x, y)
    end

fun buildfib cutoff n =
  if n <= 0
  then Leaf (sfib 20)
  else
  if n < cutoff
  then sbuildfib n
  else
    let val (x, y) = ForkJoin.par(fn _ => buildfib cutoff (n-1),
                                  fn _ => buildfib cutoff (n-1))
    in Node (n, x, y)
    end
