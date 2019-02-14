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
let () =
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