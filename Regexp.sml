use "Lang.sml";

signature REGEXP =
sig
    datatype ''a regexp =
          Const of ''a
        | One
        | Zero
        | Times of ''a regexp * ''a regexp
        | Plus of ''a regexp * ''a regexp
        | Star of ''a regexp

    val depth : ''a regexp -> int

    exception NoMatch

    val match : ''a regexp -> ''a list -> (''a list * ''a list -> 'b) -> 'b

    val LL : char regexp -> Language.language       

end

structure Regexp : REGEXP = 
struct

    datatype ''a regexp =
          Const of ''a
        | One
        | Zero
        | Times of ''a regexp * ''a regexp
        | Plus of ''a regexp * ''a regexp
        | Star of ''a regexp

    fun depth (Const(_)) = 1
      | depth One = 0
      | depth Zero = 0
      | depth (Times(r1,r2)) = 
            1 + Int.max(depth r1,depth r2)
      | depth (Plus(r1,r2)) = 
            1 + Int.max(depth r1,depth r2)
      | depth (Star(r)) = 
            1 + depth r

    exception NoMatch

(* match : ''a regexp -> ''a list -> (''a list * ''a list -> 'b) -> 'b

   REQUIRES: for all (p,s), k(p,s) either evaluates to a value or raises
   NoMatch
   ENSURES:  match r cs k  returns k(p,s),
                                    where (p,s) is the first splitting of
                                    cs (i.e. pair the first pair (p,s) with
                                    cs == p@s) such that p is in L(r) and
                                    k(p,s) does not raise NoMatch
             match r cs k  raises NoMatch if there is no such (p,s)
                                    


   COMMENTS:

    *  This definition might seem a bit abstract. Try instantiating it
    *  with this definition of k:
    *
    *  fun STRICT_ACCEPT (p,[]) = true
    *    | STRICT_ACCEPT _ = raise NoMatch
    *
    *  So match r cs STRICT_ACCEPT == true if the entire char list cs matches with r
    *  (and there's no "leftover" suffix s). More formally: cs==p@s where p
    *  in L(r) and kbool(p,s)==true.
    *  match r cs kbool raises NoMatch if cs does not match with r.

*)
    fun match (Const(a)) [] k = raise NoMatch
      | match (Const(a)) (c::cs') k = 
            if a=c
            then k([c], cs')
            else raise NoMatch
      | match (One) cs k = k([],cs)
      | match (Zero) _ _ = raise NoMatch
      | match (Times(r1,r2)) cs k = 
              match r1 cs (fn (res',cs') => 
                match r2 cs' (fn (res'',cs'') =>
                  k (res'@res'',cs'')))
      | match (Plus(r1,r2)) cs k = 
            (match r1 cs k
                handle NoMatch => match r2 cs k)
      | match (Star(r)) cs k =
            k([],cs) 
            handle NoMatch => 
                match r cs (fn (res',cs') => 
                    if (cs = cs') 
                    then raise NoMatch
                    else 
                        match (Star(r)) cs' (fn (res'',cs'') => 
                            k(res'@res'',cs'')))

    val LL = fn r => fn s => 
        match r (String.explode s) (fn (_,[]) => true | _ => raise NoMatch)
        handle NoMatch => false
end

