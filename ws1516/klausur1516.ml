open Thread
open Event

(* 3. *)

module type Vector = sig
  type t = (int*int) list
  val empty : t
  val set : int -> int -> t -> t
  val add : t -> t -> t
  val mul : int -> t -> t
  val sprod : t -> t -> int
end

module SparseVector: Vector = struct
end

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
module TreeMin: sig 
  type 'a t = Leaf of 'a | Node of 'a t * 'a t
  val min: 'a t -> 'a end = struct
  type 'a t = Leaf of 'a | Node of 'a t * 'a t
  let min tree = raise (Invalid_argument "Not implemented")
end

(* ---- Test environment ---- *)
let print_vector vector = 
  print_string "[ ";
  let rec print_rem list = 
    match list with [] -> print_string "]"; print_newline ()
                  | (i, a)::xs -> print_string "("; print_int i; print_string ", "; print_int a; print_string "), "; print_rem xs
  in print_rem vector

let () =
  print_string "Test SparseScalar\n";
  print_string "empty: "; print_vector SparseVector.empty;
  print_string "Vectors should be 10 at 1, no other entries\n";
  print_string "set: "; SparseVector.empty |> SparseVector.set 2 0 |> SparseVector.set 1 10 |> print_vector;
  print_string "add: "; SparseVector.add [(1, 3); (2, 1)] [(1, 7); (2, -1)] |> print_vector;
  print_string "mul: "; SparseVector.mul 2 [(1, 5)] |> print_vector;
  print_string "mul zero: "; SparseVector.mul 0 [(1, 5)] |> print_vector;
  print_string "sprod: "; SparseVector.sprod [(1, 5); (2, 10)] [(1, 2)] |> print_int; print_string " = 10\n";
  print_string "Test Lift List\n";
  print_string "Should print all number from 1 to 10 in ascending order\n";
  print_string "iter: "; List.of_list [1;2;3;4;5;6;7;8;9;10] |> List.iter print_int; print_newline ();
  print_string "map: "; List.of_list [0.5;1.;1.5;2.;2.5;3.;3.5;4.;4.5;5.] |> List.map (fun a -> 2.*.a) |> List.iter print_float; print_newline ();
  print_string "filter: "; List.of_list [1;2;3;20;4;5;6;7;40;8;9;11;10] |> List.filter (fun x -> x <= 10) |> List.iter print_int; print_newline ();
  print_string "append: "; List.append (List.of_list [1;2;3;4;5]) (List.of_list [6;7;8;9;10]) |> List.iter print_int; print_newline ();
  print_string "flatten: "; List.of_list [List.of_list [1;2;3;4;5]; List.of_list [6;7;8;9;10]] |> List.flatten |> List.iter print_int; print_newline (); 
  print_string "to_list/of_list: "; List.of_list [1;2;3;4;5;6;7;8;9;10] |> List.to_list |> (let rec print l = match l with x::xs -> print_int x; print xs | [] -> () in print) ; print_newline ();
  let module List = SearchTree in (
    print_string "iter: "; List.of_list [1;2;3;4;5;6;7;8;9;10] |> List.iter print_int; print_newline ();
    print_string "map: "; List.of_list [0.5;1.;1.5;2.;2.5;3.;3.5;4.;4.5;5.] |> List.map (fun a -> 2.*.a) |> List.iter print_float; print_newline ();
    print_string "filter: "; List.of_list [1;2;3;20;4;5;6;7;40;8;9;11;10] |> List.filter (fun x -> x <= 10) |> List.iter print_int; print_newline ();
    print_string "append: "; List.append (List.of_list [1;2;3;4;5]) (List.of_list [6;7;8;9;10]) |> List.iter print_int; print_newline ();
    print_string "flatten: "; List.of_list [List.of_list [1;2;3;4;5]; List.of_list [6;7;8;9;10]] |> List.flatten |> List.iter print_int; print_newline (); 
    print_string "to_list/of_list: "; List.of_list [1;2;3;4;5;6;7;8;9;10] |> List.to_list |> (let rec print l = match l with x::xs -> print_int x; print xs | [] -> () in print) ; print_newline (););
  print_string "Test minTree\n";
  let tree = TreeMin.Node (TreeMin.Node ((TreeMin.Leaf 3), (TreeMin.Leaf 5)), TreeMin.Node (TreeMin.Leaf 1, TreeMin.Leaf 10)) in TreeMin.min tree |> print_int; print_string " = 1\n";
