signature PERMUTE =
sig

(* INVARIANT: f : 'a perm must be a permutation function *)
  type 'a perm = 'a list -> 'a list

(* INVARIANT: cmp : 'a ord must be a comparison function *)
  type 'a ord = 'a * 'a -> order

(* INVARIANT: spl: 'a splitter must be a splitter function *)
  type 'a splitter = 'a list -> 'a list * 'a list

(* INVARIANT: mer: 'a merger must be a merging function *)
  type 'a merger = 'a list * 'a list -> 'a list

  (*val count : 'a ord -> ('a * 'a list -> int)
  val isIn : 'a ord -> ('a * 'a list -> bool)*)

  val rev : 'a perm 
  val riffle : 'a perm
  
  val cleanSplit : 'a splitter
  val split : 'a splitter

  val interleave : 'a merger

  val merge : 'a ord -> 'a merger
  val msort : 'a ord -> 'a perm
end

structure Permute :> PERMUTE =
struct

  type 'a perm = 'a list -> 'a list
  type 'a ord = 'a * 'a -> order
  type 'a splitter = 'a list -> 'a list * 'a list
  type 'a merger = 'a list * 'a list -> 'a list


  local
    fun trev ([],acc) = acc
      | trev (x::xs,acc) = trev(xs,x::acc)
  in
    val rev = fn L => trev(L,[])
  end

  fun len [] = 0
    | len (_::xs) = 1 + len(xs)

  fun interleave (L1,[]) = L1
    | interleave ([],L2) = L2
    | interleave (x::xs,y::ys) =
            x::y::interleave(xs,ys)
  
  fun cleanSplit L = 
    let 
      val n = (len L) div 2 
    in 
      (List.take(L,n),List.drop(L,n))
    end

  val riffle = fn L => interleave(cleanSplit L)
  
  fun split [] = ([],[])
    | split [x] = ([x],[])
    | split (x::x'::xs) = 
         let val (A,B) = split xs
         in (x::A,x'::B)
         end

  fun merge cmp (L1 : 'a list, []) = L1
    | merge cmp ([], L2 : 'a list):'a list = L2
    | merge (cmp : 'a ord) (x::xs, y::ys) =
        (case cmp(x,y) of
            GREATER => y::merge cmp (x::xs,ys)
          | _       => x::merge cmp (xs,y::ys))
  fun msort (cmp : 'a ord) [] = []
    | msort cmp ([x] : 'a list) = [x]
    | msort cmp L =
        let 
          val (A,B) = split L
        in 
          merge cmp (msort cmp A, msort cmp B)
        end
  (*
  fun msort (cmp : 'a ord) [] = []
    | msort cmp ([x] : 'a list) = [x]
    | msort cmp L =
        let
          val (A,B) = split L
          val sort = msort cmp
        in 
          merge cmp (sort A, sort B)
        end
  *)

end
