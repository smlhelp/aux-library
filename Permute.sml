signature PERMUTE =
sig

(* INVARIANT: f : 'a perm must be total and
 *        bijective on the elements of the list
 *)
  type 'a perm = 'a list -> 'a list

  val rev : 'a perm 

  val riffle : 'a perm

  type 'a ord = 'a * 'a -> order

  val msort : 'a ord -> 'a perm
end

structure Permute :> PERMUTE =
struct

  type 'a perm = 'a list -> 'a list
  type 'a ord = 'a * 'a -> order

  local
    fun trev ([],acc) = acc
      | trev (x::xs,acc) = trev(xs,x::acc)
  in
    val rev = fn L => trev(L,[])
  end

  fun len [] = 0
    | len (_::xs) = 1 + len(xs)

local
  fun interleave (L1,[]) = L1
    | interleave ([],L2) = L2
    | interleave (x::xs,y::ys) =
            x::y::interleave(xs,ys)
  fun cleanSplit L = 
    let val n = (len L) div 2 
    in (List.take(L,n),List.drop(L,n))
    end
in
  val riffle = fn L => interleave(cleanSplit L)
end
local
  fun split [] = ([],[])
    | split [x] = ([x],[])
    | split (x::x'::xs) = 
         let val (A,B) = split xs
         in (x::A,x'::B)
         end
  fun merge (_, L1 : 'a list, []) = L1
    | merge (_, [], L2 : 'a list): 'a list = L2
    | merge (cmp : 'a ord, x::xs, y::ys)=
        (case cmp(x,y) of
            GREATER => y::merge(cmp,x::xs,ys)
          | _       => x::merge(cmp,xs,y::ys))
in
  fun msort cmp = 
    fn []  => [] 
     | [x] => [x]
     | L   => 
        let val (A,B) = split L
        in merge(cmp, msort cmp A, msort cmp B)
        end
end

end
