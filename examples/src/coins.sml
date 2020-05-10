(* https://github.com/ghc/nofib/blob/f481777acf608c132db47cb8badb618ef39a0d6f/parallel/coins/coins.hs *)

structure L = List
structure W = Word

datatype AList
  = ANil
  | ASing of (int L.list)
  | Append of (AList * AList)

fun lenA (ls : AList) : int =
  case ls of
    ANil => 0
  | ASing _ => 1
  | Append (l, r) => lenA l + lenA r

fun append (ls1 : AList) (ls2 : AList) : AList =
  case (ls1,ls2) of
    (ANil, r) => r
  | (l, ANil) => l
  | (l, r) => Append (l, r)

type coin = int * int

fun spayA (amt : int) (coins : coin L.list) (acc : int L.list) : AList =
  if amt = 0
  then ASing acc
  else
    case coins of
      ((c,q) :: coins_rst) =>
      if c > amt
      then spayA amt coins_rst acc
      else
        let
          val coins1 = if q = 1 then coins_rst else (c,q-1) :: coins_rst
          val left = spayA (amt - c) coins1 (c :: acc)
          val right = spayA amt coins_rst acc
        in
          append left right
        end
    | [] => ANil

fun spayA' (amt : W.word) (coins : coin L.list) (acc : int L.list) : AList =
  spayA (W.toInt amt) coins acc

fun ppayA (depth : int) (amt : int) (coins : coin L.list) (acc : int L.list) : AList =
  if depth = 0
  then spayA amt coins acc
  else if amt = 0
  then ASing acc
  else
    case coins of
      ((c,q) :: coins_rst) =>
      if c > amt
      then spayA amt coins_rst acc
      else
        let
          val (coins1,depth1) = if q = 1
                                then (coins_rst, depth - 1)
                                else ((c,q-1) :: coins_rst, depth)
          val (left, right) = ForkJoin.par
                                ( fn _ => ppayA depth1 (amt - c) coins1 (c :: acc)
                                , fn _ => ppayA depth amt coins_rst acc
                                )
        in
          append left right
        end
    | [] => ANil

fun ppayA' (depth : int) (amt : W.word) (coins : coin L.list) (acc : int L.list) : AList =
  ppayA depth (W.toInt amt) coins acc

val coins_input : coin list =
  let
    val cs = [250, 100, 25, 10, 5, 1]
    val qs = [55, 88, 88, 99, 122, 177]
  in
    ListPair.zip (cs, qs)
  end
