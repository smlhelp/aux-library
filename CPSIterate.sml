signature CPS_ITERATE =
sig

datatype result = Accept 
                | Keep 
                | Discard 
                | Break of string

val For :    ('a -> result)
          -> 'a list
          -> ('a -> 'b -> 'b)
          -> 'b
          -> ('a -> 'c)
          -> (string -> 'c)
          -> ('b -> 'c)
          -> 'c
val iFor :   (int * 'a -> result)
          -> 'a list
          -> (int * 'a -> 'b -> 'b)
          -> 'b
          -> (int * 'a -> 'c)
          -> (string -> 'c)
          -> ('b -> 'c)
          -> 'c

datatype command = Stop
                 | Continue
                 | Crash of string

val While :   ('a -> command)
           -> ('a -> ('a -> 'b) -> 'b)
           -> ('a -> 'b)
           -> (string -> 'b)
           -> 'a
           -> 'b

val iWhile :  (int * 'a -> command)
           -> (int * 'a -> ('a -> 'b) -> 'b)
           -> (int * 'a -> 'b)
           -> (string -> 'b)
           -> 'a
           -> 'b
end

structure CPSIterate : CPS_ITERATE =
struct

datatype result = Accept 
                | Keep 
                | Discard 
                | Break of string

fun For (check : 'a -> result)
        (L : 'a list)
        (combine : 'a -> 'b -> 'b)
        (base : 'b)
        (success : 'a -> 'c)
        (panic : string -> 'c)
        (return : 'b -> 'c)
        : 'c
 =
  let
    fun run ([] : 'a list) (k:'b -> 'c) : 'c =
          k base
      | run (x::xs) k =
          (case (check x) of
              Accept => success x
          |     Keep => run xs (k o (combine x))
          |  Discard => run xs k
          | (Break s)=> panic s)
  in
    run L return
  end

fun iFor (check : int * 'a -> result)
        (L : 'a list)
        (combine : int * 'a -> 'b -> 'b)
        (base : 'b)
        (success : int * 'a -> 'c)
        (panic : string -> 'c)
        (return : 'b -> 'c)
        : 'c
 =
  let
    fun run _ ([] : 'a list) (k:'b -> 'c) : 'c =
          k base
      | run n (x::xs) k =
          (case (check(n,x)) of
              Accept => success(n,x)
          |     Keep => run (n+1) xs (k o (combine (n,x)))
          |  Discard => run (n+1) xs k
          | (Break s)=> panic s)
  in
    run 0 L return
  end

datatype command = Stop
                 | Continue
                 | Crash of string

fun While (guard : 'a -> command)
          (step : 'a -> ('a -> 'b) -> 'b)
          (return : 'a -> 'b)
          (panic : string -> 'b)
          (init : 'a)
          :'b = 
        case (guard init) of
             Stop => return init
           | Continue => step init (While guard step return panic)
           | (Crash s) => panic s

fun iWhile (guard : int * 'a -> command)
          (step : int * 'a -> ('a -> 'b) -> 'b)
          (return : int * 'a -> 'b)
          (panic : string -> 'b)
          (init : 'a)
          :'b = 
      let
        fun WhileN n x = 
        case (guard (n,x)) of
             Stop => return (n,x)
           | Continue => step (n,x) (WhileN (n+1))
           | (Crash s) => panic s
      in
        WhileN 0 init
      end
end

