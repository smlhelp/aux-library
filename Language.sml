
signature LANGUAGE = 
sig

  type 'S language = 'S list -> bool

  val everything : 'S language
  val nothing : 'S language
  val just : ''S list list -> ''S language

  val Or 
    : 'S language * 'S language -> 'S language
  val And 
    : 'S language * 'S language -> 'S language
  val Not 
    : 'S language -> 'S language
  val Xor 
    : 'S language * 'S language -> 'S language

  val lengthEqual : int -> 'S language
  val lengthLess : int -> 'S language
  val lengthGreater : int -> 'S language

  val Sub : ''S list -> ''S language
  val Sup : ''S list -> ''S language

  val str : char language -> string -> bool 
end

structure Language :> LANGUAGE =
struct

  type 'S language = 'S list -> bool
  
  fun everything (x:'S list) = true
  fun nothing (x : 'S list) = false

  fun just ([] : ''S list list) s = false
    | just (x::xs) s = (s=x) orelse just xs s

  fun Or (L1,L2) s = (L1 s) orelse (L2 s) 
  fun And (L1,L2) s = (L1 s) andalso (L2 s)
  fun Not L = not o L
  fun Xor (L1,L2) s = (L1 s) <> (L2 s)


  fun lengthEqual n s = (List.length s)=n
  fun lengthLess n s = (List.length s)<n
  fun lengthGreater n s = (List.length s)>n


  fun isPrefix [] _ = true
    | isPrefix _ [] = false
    | isPrefix (c::cs) (c'::cs') =
            (c=c') andalso (isPrefix cs cs')
  fun isSubList [] _ = true
    | isSubList _ [] = false
    | isSubList sub (c::cs) = 
        (isPrefix sub (c::cs)) 
        orelse (isSubList sub cs)

  fun Sub s0 s = isSubList s s0
  fun Sup s0 s = isSubList s0 s

  fun str L = L o String.explode

end
