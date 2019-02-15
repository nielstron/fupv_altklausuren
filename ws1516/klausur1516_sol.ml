open Thread
open Event

(* 1. *)
let f x = let p a b = a+b in p x
let f = match [1,2] with [x,y] -> x+y | z -> 0
(* let f = List.fold_left (fun a x -> x a) *)
let f = let rec x = 1 in let x x = x in (print_string "Expression of task 1 2e): "; print_int (x 2); print_newline ();)

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
  type t = (int*int) list
  let empty = []
  let rec set i a v = match v with 
    | [] -> if a = 0 then v else (i,a)::v
    | (j, b)::xs -> if j = i then (if a = 0 then xs else (i, a)::xs) else (j,b)::(set i a xs)
  let rec add v w = 
    let rec add_elem (i, a) w = 
      match w with
      | [] -> [(i, a)]
      | (j, b)::xs -> if j = i then set i (a+b) xs else (j,b)::(add_elem (i,a) xs)
    in match v with [] -> w
                  | y::ys -> add ys (add_elem y w)
  let rec mul r v = match v with [] -> [] | (i, a)::xs -> set i (r*a) (mul r xs) 
  let rec sprod v w = let rec sprod_elem (i, a) w =
                        match w with 
                        | [] -> 0
                        | (j, b)::xs -> if j = i then a*b else sprod_elem (i, a) xs
    in match v with [] -> 0 | x::xs -> sprod_elem x w + sprod xs w
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
  type 'a t = 'a B.t
  let fold = B.fold
  let empty = B.empty
  let insert = B.insert
  let iter f l = B.fold (fun a () -> f a; ()) l ()
  let rev l = B.fold (fun a sl -> B.insert a sl) l B.empty
  let map f l = rev (B.fold (fun a sl -> B.insert (f a) sl) l B.empty)
  let filter f l = rev (B.fold (fun a sl -> if (f a) then B.insert a sl else sl) l B.empty)
  let append l r = B.fold (fun a sl -> B.insert a sl) (rev l) r
  let flatten t = B.fold (fun a sl -> append a sl) (rev t) B.empty
  let to_list l = B.fold (fun a sl -> a::sl) (rev l) []
  let rec of_list l = match l with [] -> B.empty | x::xs -> B.insert x (of_list xs)
end

module List = Lift (struct
    (* Code Aufgabe 2 *)
    type 'a t = 'a list
    let empty = []
    let insert a l = a::l
    let rec fold f l b = match l with
      | [] -> b
      | x::xs -> fold f xs (f x b)
  end)

module SearchTree = Lift (struct
    (* Code Bonus-Aufgabe 3 *)
    type 'a t = Node of ('a t*'a*'a t) | Empty
    let empty = Empty
    let rec insert a l = match l with Empty -> Node (Empty, a, Empty)
                                    | Node (l, v, r) -> if a < v then Node (insert a l, v, r) else Node (l, v, insert a r)
    let rec fold f l b = match l with Empty -> b 
                                    | Node (l, v, r) -> fold f r (f v (fold f l b))
  end)


(* 5. *)
type 'a t = Leaf of 'a | Node of 'a t * 'a t

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
  print_string "Test Lift SearchTree\n";
  print_string "Should print all number from 1 to 10 in ascending order\n";
  let module List = SearchTree in (
    print_string "iter: "; List.of_list [1;2;3;4;5;6;7;8;9;10] |> List.iter print_int; print_newline ();
    print_string "map: "; List.of_list [0.5;1.;1.5;2.;2.5;3.;3.5;4.;4.5;5.] |> List.map (fun a -> 2.*.a) |> List.iter print_float; print_newline ();
    print_string "filter: "; List.of_list [1;2;3;20;4;5;6;7;40;8;9;11;10] |> List.filter (fun x -> x <= 10) |> List.iter print_int; print_newline ();
    print_string "append: "; List.append (List.of_list [1;2;3;4;5]) (List.of_list [6;7;8;9;10]) |> List.iter print_int; print_newline ();
    print_string "flatten: "; List.of_list [List.of_list [1;2;3;4;5]; List.of_list [6;7;8;9;10]] |> List.flatten |> List.iter print_int; print_newline (); 
    print_string "to_list/of_list: "; List.of_list [1;2;3;4;5;6;7;8;9;10] |> List.to_list |> (let rec print l = match l with x::xs -> print_int x; print xs | [] -> () in print) ; print_newline ();)