open Thread
open Event

(* 4. *)

module type Base = sig
  type 'a t
  val empty : 'a t (* nullary/nonrec. constr. *)
  val insert : 'a -> 'a t -> 'a t (* rec. constr. *)
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Lift (B : Base) : sig
  include Base
  val iter : ('a -> unit) -> 'a t
    -> unit
  val map : ('a -> 'b) -> 'a t ->
    'b t
  val filter : ('a -> bool) -> 'a
      t -> 'a t
  val append : 'a t -> 'a t -> 'a
      t
  val flatten : 'a t t -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end = struct
  (* Code Aufgabe 1 *)
end

module List = Lift (struct
    (* Code Aufgabe 2 *)
  end)

module SearchTree = Lift (struct
    (* Code Bonus-Aufgabe 3 *)
  end)


(* 5. *)
type 'a t = Leaf of 'a | Node of 'a t * 'a t

val min : 'a t -> 'a
