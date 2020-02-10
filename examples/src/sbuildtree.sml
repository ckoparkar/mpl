datatype tree = Leaf of int
              | Node of int * tree * tree;
 
fun sumtree tr =
  case tr of
    Leaf (i)       => i
  | Node (n, x, y) => n + (sumtree x) + (sumtree y)

fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun sbuildtree n =
  if n <= 0
  then Leaf (sfib (20))
  else
    let val (x, y) = (sbuildtree (n-1), sbuildtree (n-1))
    in Node (n, x, y)
    end 

val n = CommandLineArgs.parseInt "N" 10
val _ = print ("sbuildtree " ^ Int.toString n ^ "\n")

val t0 = Time.now ()
val result = sbuildtree n
val t1 = Time.now ()

val sum = sumtree result

val _ = print ("finished in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val _ = print ("result " ^ Int.toString sum ^ "\n")
