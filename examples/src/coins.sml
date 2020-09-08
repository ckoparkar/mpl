(* https://github.com/ghc/nofib/blob/f481777acf608c132db47cb8badb618ef39a0d6f/parallel/coins/coins.hs *)

structure L = List

datatype AList
  = ANil
  | ASing of int
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

fun payA_seq (amt : int) (coins : coin L.list) : AList =
  if amt = 0
  then ASing 1
  else
    case coins of
      ((c,q) :: coins_rst) =>
      if c > amt
      then payA_seq amt coins_rst
      else
        let
          val coins1 = if q = 1 then coins_rst else (c,q-1) :: coins_rst
          val left = payA_seq (amt - c) coins1
          val right = payA_seq amt coins_rst
        in
          append left right
        end
    | [] => ANil

fun payA_par (depth : int) (amt : int) (coins : coin L.list) : AList =
  if depth = 0
  then payA_seq amt coins
  else if amt = 0
  then ASing 1
  else
    case coins of
      ((c,q) :: coins_rst) =>
      if c > amt
      then payA_seq amt coins_rst
      else
        let
          val (coins1,depth1) = if q = 1
                                then (coins_rst, depth - 1)
                                else ((c,q-1) :: coins_rst, depth)
          val (left, right) = ForkJoin.par
                                ( fn _ => payA_par depth1 (amt - c) coins1
                                , fn _ => payA_par (depth-1) amt coins_rst
                                )
        in
          append left right
        end
    | [] => ANil

val coins_input : coin list =
  let
    val cs = [250, 100, 25, 10, 5, 1]
    val qs = [55, 88, 88, 99, 122, 177]
  in
    ListPair.zip (cs, qs)
  end
