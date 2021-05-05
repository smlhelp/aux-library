
signature LANGUAGE = 
sig

    type language = string -> bool

    val everything : language
    val nothing : language
    val just : string list -> language

    val Or : language * language -> language
    val And : language * language -> language
    val Not : language -> language
    val Xor : language * language -> language

    val lengthEqual : int -> language
    val lengthLess : int -> language
    val lengthGreater : int -> language

    val substrings : string -> language
    val superstrings : string -> language

end


structure Language :> LANGUAGE =
struct

    type language = string -> bool

    val everything : language = fn _ => true
    val nothing : language = fn _ => false
    fun just [] s = false
      | just (x::xs) s = (s=x) orelse just xs s

    fun Or (L1,L2) s = (L1 s) orelse (L2 s) 
    fun And (L1,L2) s = (L1 s) andalso (L2 s)
    val Not : language -> language = Fn.curry (op o) not
    fun Xor (L1,L2) s = (L1 s) <> (L2 s)


    fun lengthEqual n s = (String.size s)=n
    fun lengthLess n s = (String.size s)<n
    fun lengthGreater n s = (String.size s)>n


    fun isPrefix [] _ = true
      | isPrefix _ [] = false
      | isPrefix (c::cs) (c'::cs') =
            (c=c') andalso (isPrefix cs cs')
    fun isSubList [] _ = true
      | isSubList _ [] = false
      | isSubList sub (sup as c::cs) = 
           (isPrefix sub sup) orelse (isSubList sub cs)

    fun substrings s0 s = isSubList (String.explode s) (String.explode s0)
    fun superstrings s0 s = isSubList (String.explode s0) (String.explode s)

end
