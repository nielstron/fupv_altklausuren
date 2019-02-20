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
module Memo2 (A:Hashable) (B:Hashable): sig
  type 'a s (* state for results of type 'a *)
  val create : (A.t -> B.t -> 'a) -> 'a s
  val eval : A.t -> B.t -> 'a s -> 'a * 'a s (* result and new state *)
  val (%>) : ('a s -> 'a * 'a s) -> ('a -> 'a s -> 'b) -> 'a s -> 'b
end = struct
end


(* --- Test environment --*)

let () =
  print_string "Test server\n";
  let s = Server.serve (fun (x, s) -> delay x; print_string s; print_newline ()) in
  let t1 = create (fun _ -> Server.request s (0.5, "I was requested first but should be printed second")) () in 
  let t2 = create (fun _ -> delay 0.1; Server.request s (0.1, "I was requested second but should be printed first")) () in
  join t1; join t2;
  print_string "Test compressed trees\n";
  let module CTreeTest = struct
    open CompressedTree
    let rec print_ctree ct =  
      print_string "(";
      (match ct with | CLeaf -> ()
                     | Eq (v, sct) -> Printf.printf "%d, Eq " v;
                       print_ctree sct;
                     | Uneq (v, lct, rct) -> Printf.printf "%d, Uneq " v;
                       print_ctree lct;
                       print_string ", ";
                       print_ctree rct
      );
      print_string ")"
    let test () = 
      print_string "compress and count: \n";
      let tree = compress (Node (1, Leaf, Leaf)) in
      print_ctree tree; print_string " = (1, Eq ())\n";
      print_int (count tree); print_string " = 1\n";
      let tree = compress (Node (1, Node (2, Leaf, Node (3, Leaf, Leaf)), Leaf)) in
      print_ctree tree; print_string " = (1, Uneq (2, Uneq (), (3, Eq ())), ())\n";
      print_int (count tree); print_string " = 3\n";
      print_string "merge: \n";
      let tree = merge ( + ) tree (compress (Node (1, Leaf, Node (2, Leaf, Node (3, Leaf, Leaf))))) in
      print_ctree tree; print_string " = (2, Eq (2, Uneq (), (3, Eq ())))\n";
      print_int (count tree); print_string " = 5\n";
  end in CTreeTest.test ();
  print_string "Test Memo2\n";
  let module IntFork = struct
    type t = int
    let hash i = i mod 2
    let create i = i
  end
  in let module MemoIntforkTuple = Memo2 (IntFork) (IntFork) in
  let f a b = print_string "This string exactly printed thrice\n"; a+b in
  let a = IntFork.create 1 in let b = IntFork.create 2 in
  let c = IntFork.create 3 in let d = IntFork.create 2 in (*same hash but different val*)
  let e = IntFork.create 2 in let g = IntFork.create 2 in (*different hash --> tree structure *)
  let m = MemoIntforkTuple.create f in
  let (r1, m ) = MemoIntforkTuple.eval a b m in
  let (r2, m ) = MemoIntforkTuple.eval a b m in
  let (r3, m ) = MemoIntforkTuple.eval c d m in
  let (r4, m ) = MemoIntforkTuple.eval c d m in
  let (r5, m ) = MemoIntforkTuple.eval e g m in
  let (r6, m ) = MemoIntforkTuple.eval e g m in
  Printf.fprintf stdout "%d = %d\n" r1 3;
  Printf.fprintf stdout "%d = %d\n" r2 3;
  Printf.fprintf stdout "%d = %d\n" r3 5;
  Printf.fprintf stdout "%d = %d\n" r4 5;
  Printf.fprintf stdout "%d = %d\n" r5 4;
  Printf.fprintf stdout "%d = %d\n" r6 4;
  ();
