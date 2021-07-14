use "Language.sml";

signature REGEXP =
sig
    datatype ''S regexp =
          Const of ''S
        | One
        | Zero
        | Plus of ''S regexp * ''S regexp
        | Times of ''S regexp * ''S regexp
        | Star of ''S regexp

    val depth : ''S regexp -> int
   
    exception NoMatch
    
    val match : ''S regexp -> ''S list -> (''S list * ''S list -> 'b) -> 'b

    val LL : ''S regexp -> ''S Language.language       
  
    val reduce : ''S regexp -> ''S regexp
    
    val represent : (''S -> string) -> ''S regexp -> string
    val printRep : (''S -> string) -> ''S regexp -> unit

end
structure Regexp : REGEXP = 
struct
  datatype ''S regexp =
      Zero
      | One
      | Const of ''S
      | Plus of ''S regexp * ''S regexp
      | Times of ''S regexp * ''S regexp
      | Star of ''S regexp

  fun depth Zero = 0
    | depth One = 0
    | depth (Const(_)) = 0
    | depth (Plus(R1,R2)) = 
        1 + Int.max(depth R1,depth R2)
    | depth (Times(R1,R2)) = 
        1 + Int.max(depth R1,depth R2)
    | depth (Star(R)) = 
        1 + depth R
  
  exception NoMatch
 
  fun match Zero _ _ = raise NoMatch
    | match One cs k = k([],cs)
    | match (Const(c)) [] k = raise NoMatch
    | match (Const(c)) (c'::cs') k = 
        if c=c'
        then k([c'], cs')
        else raise NoMatch
    | match (Plus(R1,R2)) cs k = 
          (match R1 cs k
            handle NoMatch => match R2 cs k)
    | match (Times(R1,R2)) cs k = 
        match R1 cs (fn (res',cs') => 
          match R2 cs' (fn (res'',cs'') =>
            k (res'@res'',cs'')))
    | match (Star(R)) cs k =
        k([],cs) 
          handle NoMatch => 
            match R cs (fn (res',cs') => 
              if (cs = cs') 
              then raise NoMatch
              else 
                match (Star(R)) cs' (fn (res'',cs'') => 
                  k(res'@res'',cs'')))

  val LL = fn R => fn s => 
    match R s (fn (_,[]) => true | _ => raise NoMatch)
    handle NoMatch => false

  fun reduce Zero = Zero
    | reduce One = One
    | reduce (Const c) = Const c
    | reduce (Plus(R1,R2)) =
        (case (reduce R1,reduce R2) of
          (Zero,rR2) => rR2
        | (rR1,Zero) => rR1
        | (rR1,rR2) => Plus(rR1,rR2))
    | reduce (Times(R1,R2)) =
        (case (reduce R1,reduce R2) of
          (Zero,_) => Zero
        | (_,Zero) => Zero
        | (One,rR2) => rR2
        | (rR1,One) => rR1
        | (rR1,rR2) => Times(rR1,rR2))
    | reduce (Star R) =
        (case (reduce R) of
           Zero => One
         | One => One
         | rR => Star(rR))

  (* REQURES: R contains no instances of Zero *)
  fun represent toStr Zero = raise Fail "Cannot represent Zero"
    | represent toStr One = "(.{0,0})"
    | represent toStr (Const c) = (toStr c)
    | represent toStr (Plus(R1,R2)) =
        "(" ^ (represent toStr R1) ^ "|" ^ (represent toStr R2) ^ ")"
    | represent toStr (Times(R1,R2)) =
        (represent toStr R1)^(represent toStr R2)
    | represent toStr (Star(R)) = 
        "(" ^ (represent toStr R) ^ ")*"

  fun printRep toStr R = print((represent toStr (reduce R))^"\n")


end

