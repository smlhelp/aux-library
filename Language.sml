
signature LANGUAGE = 
sig

  type 'S language = 'S list -> bool

  val everything : 'S language
  val nothing : 'S language
  val singleton : ''S list -> ''S language
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

  val str : char language -> string -> bool 
end

structure Language :> LANGUAGE =
struct

  type 'S language = 'S list -> bool
  
  fun everything (x:'S list) = true
  fun nothing (x : 'S list) = false

  val singleton = Fn.equal
  fun just ([] : ''S list list) s = false
    | just (x::xs) s = (s=x) orelse just xs s

  fun Or (L1,L2) s = (L1 s) orelse (L2 s) 
  fun And (L1,L2) s = (L1 s) andalso (L2 s)
  fun Not L = not o L
  fun Xor (L1,L2) s = (L1 s) <> (L2 s)

  fun lengthEqual n s = (List.length s)=n
  fun lengthLess n s = (List.length s)<n
  fun lengthGreater n s = (List.length s)>n

  fun str L = L o String.explode

end
