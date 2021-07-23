functor mkEq (eqtype t) : EQ =
struct
  type t = t
  val equal = Fn.equal
end

structure IntEq = mkEq(type t = int)
structure BoolEq = mkEq(type t = bool)
structure StringEq = mkEq(type t = string)
structure IntListEq = mkEq(type t = int list)
structure IntTreeEq = mkEq(type t = int Tree.tree)

structure IntOrd : ORD =
struct
  type t = int
  val cmp = Int.compare
end
structure stringOrd : ORD =
struct
  type t = string
  val cmp = String.compare
end

functor cmpEqual (K : ORD):EQ =
struct
  type t = K.t
  fun equal x y = K.cmp(x,y) = EQUAL
end

functor Fst ( structure T1 : ORD
              type t2
            ) : ORD =
struct
  type t = T1.t * t2
  
  val fst : t -> T1.t = fn (a,b) => a
  val fstPair = fn (P1,P2) => (fst P1,fst P2)
  val cmp = T1.cmp o fstPair
end
functor Snd ( type t1
              structure T2 : ORD
            ) : ORD =
struct
  type t = t1 * T2.t 
  
  val snd : t -> T2.t = fn (a,b) => b
  val sndPair = fn (P1,P2) => (snd P1,snd P2)
  val cmp = T2.cmp o sndPair
end

functor Lex ( structure T1 : ORD
              structure T2 : ORD
            ) : ORD =
struct
  type t = T1.t * T2.t

  fun cmp ((x1,x2),(y1,y2)) = 
       case T1.cmp(x1,y1) of
          EQUAL => T2.cmp(x2,y2)
        | O => O
end

functor ListSet (Elt : EQ):>SET
where type Elt.t = Elt.t =
struct
  structure Elt = Elt

  (* INVARIANT: S : Set contains no duplicates
  *  (according to Elt.equal)   *)
  type Set = Elt.t list

  val empty = []

  fun lookup [] y = NONE
    | lookup (x::xs) y = 
        if (Elt.equal x y)
        then SOME(x)
        else lookup xs y

  exception ExistingElt
  fun insert (y,S) =
    case lookup S y of
        NONE => y::S
      | (SOME _) => raise ExistingElt

  fun remove (y,L) = List.filter (not o (Elt.equal y)) L

  fun overwrite (y,S) = y::remove(y,S)

  val union = List.foldr insert

  fun toString toStr S =
    "{" ^ (String.concatWith "," (map toStr S)) ^ "}"
end

functor OrdListSet (EltOrd : ORD):> SET 
where type Elt.t = EltOrd.t=
struct
  structure Elt = cmpEqual(EltOrd)

  (* INVARIANT: S : Set contains no duplicates
  *  (according to Elt.equal) and is sorted
  *  (according to EltOrd.cmp)                 *)
  type Set = Elt.t list

  val empty = []

  fun lookup [] y = NONE
    | lookup (x::xs) y = 
        case EltOrd.cmp(y,x) of
          LESS => NONE
        | EQUAL => SOME(x)
        | GREATER => lookup xs y

  exception ExistingElt
  fun insert (y,[]) = [y]
    | insert (y,(x::xs)) = 
        case EltOrd.cmp(y,x) of
          LESS  => y::x::xs
        | EQUAL => raise ExistingElt
        | GREATER => x::insert(y,xs)

  fun overwrite (y,[]) = [y]
    | overwrite (y,(x::xs)) = 
        case EltOrd.cmp(y,x) of
          LESS => y::x::xs
        | EQUAL => y::xs
        | GREATER => x::overwrite(y,xs)

  fun remove (y,[]) = []
    | remove (y,(x::xs)) = 
        case EltOrd.cmp(y,x) of
          LESS => x::xs
        | EQUAL => xs
        | GREATER => x::remove(y,xs)

  val union = List.foldr insert

  fun toString toStr S =
    "{" ^ (String.concatWith "," (map toStr S)) ^ "}"
end
  

functor OrdTreeSet (EltOrd : ORD):> SET 
where type Elt.t = EltOrd.t =
struct
  structure Elt = cmpEqual(EltOrd)

  (* INVARIANT: S : Set contains no duplicates
  *  (according to Elt.equal) and is sorted
  *  (according to EltOrd.cmp)                 *)
  type Set = Elt.t Tree.tree

  open Tree

  val empty = Empty

  fun lookup Empty y = NONE
    | lookup (Node(L,x,R)) y = 
        case EltOrd.cmp(y,x) of
          LESS => lookup L y
        | EQUAL => SOME(x)
        | GREATER => lookup R y

  fun singleton x = Node(Empty,x,Empty)


  exception ExistingElt
  fun insert (y,Empty) = singleton y
    | insert (y,Node(L,x,R)) = 
        case EltOrd.cmp(y,x) of
          LESS => Node(insert(y,L),x,R)
        | EQUAL => raise ExistingElt
        | GREATER => Node(L,x,insert(y,R))

  fun overwrite (y,Empty) = singleton y
    | overwrite (y,Node(L,x,R)) = 
        case EltOrd.cmp(y,x) of
          LESS => Node(overwrite(y,L),x,R)
        | EQUAL => Node(L,y,R)
        | GREATER => Node(L,x,overwrite(y,R))

  fun splitAt y Empty = (Empty,Empty)
    | splitAt y (Node(L,x,R)) =
        case EltOrd.cmp(y,x) of
          LESS => let
                   val (t1,t2) = splitAt y L
                  in
                    (t1,Node(t2,x,R))
                  end
        | EQUAL => (L,R)
        |  _   =>
                  let
                    val (t1,t2) = splitAt y R
                  in
                    (Node(L,x,t1),t2)
                  end

  fun union Empty S2 = S2
    | union S1 Empty = S1
    | union (Node(L1,x1,R1)) S2 = 
       let
          val (L2,R2) = splitAt x1 S2
       in
          Node(union L1 L2,x1,union R1 R2)
       end

  fun remove (y,Empty) = Empty
    | remove (y,Node(L,x,R)) = 
        case EltOrd.cmp(y,x) of
          LESS => Node(remove(y,L),x,R)
        | EQUAL => union L R
        | GREATER => Node(L,x,remove(y,R))


  fun toString toStr S =
    "{" ^ 
    (String.concatWith "," (map toStr (inord S))) 
    ^ "}"
end





