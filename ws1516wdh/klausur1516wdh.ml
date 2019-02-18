open Thread
open Event

(* 3. *)

module CompressedTree = struct
  type 'a t = Leaf | Node of 'a * 'a t * 'a t
  type 'a c = CLeaf | Uneq of 'a * 'a c * 'a c | Eq of 'a * 'a c

  let rec compress t = Invalid_argument "TODO" |> raise

  let rec count t = Invalid_argument "TODO" |> raise

  let rec merge f a b = Invalid_argument "TODO" |> raise
end

(* 4. *)
module Server : sig
  type ('a, 'b) t
  val serve : ('a -> 'b) -> ('a, 'b) t
  val request : ('a, 'b) t -> 'a -> 'b
end = struct
end

(* 6. *)
module type Hashable = sig
  type t
  val hash : t -> int
end

(* 6.1 *)

module Memo (A: Hashable) : sig
  type 'a s (* state for results of type 'a *)
  val create : (A.t -> 'a) -> 'a s
  val eval : A.t -> 'a s -> 'a * 'a s (* result and new state *)
  val (%>) : ('a s -> 'a * 'a s) -> ('a -> 'a s -> 'b) -> 'a s -> 'b
end = struct
  type 'a tree = Node of int * (A.t * 'a) list * 'a tree * 'a tree | Empty
  type 'a s = (A.t -> 'a) * 'a tree
  let create = Invalid_argument "TODO" |> raise
  let eval = Invalid_argument "TODO" |> raise
  let (%>) = Invalid_argument "TODO" |> raise

end

(* 6.2 *)
module Int: Hashable = struct
end

(* 6.3 *)
module Tuple (A:Hashable) (B:Hashable) : Hashable = struct
end

(* 6.4 *)
module Memo2 (A:Hashable) (B:Hashable) = struct
end


(* --- Test environment --*)

let () =
  print_string "Test server\n";
  let s = Server.serve (fun (x, s) -> delay x; print_string s; print_newline ()) in
  let t1 = create (fun _ -> Server.request s (0.5, "I was requested first but should be printed second")) () in 
  let t2 = create (fun _ -> delay 0.1; Server.request s (0.1, "I was requested second but should be printed first")) () in
  join t1; join t2;