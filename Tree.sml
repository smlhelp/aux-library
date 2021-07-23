structure Tree = 
struct
  datatype 'a tree = Empty 
                   | Node of 'a tree * 'a * 'a tree
  fun foldr g z Empty = z
    | foldr g z (Node(L,x,R)) =
        foldr g (g(x,foldr g z R)) L
  
  fun inord Empty = []
    | inord (Node(L,x,R)) = 
        (inord L)@(x::inord R)

  fun reduce g z Empty = z
    | reduce g z (Node(L,x,R)) = 
        g(reduce g z L,g(x,reduce g z R))
  
  fun size Empty = 0
    | size (Node(L,_,R)) = 
        1 + size L + size R

  fun depth Empty = 0
    | depth (Node(L,_,R)) =
        1 + Int.max(depth L, depth R)

end


