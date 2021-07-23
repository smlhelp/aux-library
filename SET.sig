signature EQ =
sig
  type t
  (* INVARIANT: equal is a reasonable notion of equality *)
  val equal : t -> t -> bool
end

signature ORD =
sig
  type t

  (* INVARIANT: equal is a comparison function *)
  val cmp : t * t -> order
end

signature SET =
sig
  structure Elt : EQ
  
  type Set

  val empty : Set

  exception ExistingElt  
  val insert : Elt.t * Set  -> Set
  val overwrite : Elt.t * Set  -> Set
  
  val remove : Elt.t * Set -> Set
  
  val lookup : Set -> Elt.t -> Elt.t option

  val union : Set -> Set -> Set

  val toString : (Elt.t -> string) -> Set -> string
end


