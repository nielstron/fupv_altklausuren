open Thread
open Event

(* 1.6 *)

(* 1.7 *)


(* 2.5. *)

let rec f a b c =
  if c = 0 then 1 else a * b * f a b (c-1)


(* 5. *)
module Trie = struct
  type trie = Node of bool * (char * trie) list

  (* Source: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

  (* 5.1. *)


  (* 5.2. *)

end
(* 6 *)

module type Graph = sig
  type node
  type graph
  (* Gibt die Liste direkter Nachfolger des gegebenen Knotens zurück *)
  val successors : graph -> node -> node list
end

(* 6.a *)

module BinaryTree = struct
end

(* 6.b *)
module GraphImpl = struct
end

module MakeGraphSearch (G:Graph): sig
  val dfs : G.graph -> G.node -> ('a -> G.node -> 'a) -> 'a -> 'a 
end = struct
end

(* 7 *)
let map_reduce f g l =
  let la = List.map f l in
  List.fold_left g (List.hd l) (List.tl la)

module type ParallelReduceType = sig
  val pmap : ('a -> 'b) -> 'a list -> 'b event list
  val preduce : ('a -> 'a -> 'c) -> 'a -> 'a -> 'c event
  val reduce_list : ('a -> 'a -> 'a) -> 'a event list -> 'a
  val map_reduce: ('a -> 'b)-> ('b -> 'b -> 'b) -> 'a list -> 'b
end

module ParallelReduce = struct

  (* 7.1. *)

  (* 7.2. *)

  (* 7.3. *)

  (* 7.4. *)

end


(* --- Test environment ---*)

let print_char_list list = 
  print_string "[";
  let rec print_rem list = 
    match list with [] -> print_string "]"; print_newline ()
                  | x::xs -> print_char x; print_rem xs
  in print_rem list

let print_trie tree = let rec iter_words tree cur = 
                        let Trie.Node (b, children) = tree in
                        (if b then print_char_list cur;
                         let rec iter_children childs cur = 
                           match childs with 
                           | [] -> ()
                           | (c, sub)::xs -> iter_words sub (cur@[c]); iter_children xs cur
                         in iter_children children cur)
  in iter_words tree []

let print_list list = 
  print_string "[ ";
  let rec print_rem list = 
    match list with [] -> print_string "]"; print_newline ()
                  | x::xs -> print_int x; print_string ", "; print_rem xs
  in print_rem list

let print_list_gen f list =
  print_string "[ ";
  let rec print_rem list = 
    match list with [] -> print_string "]";
                  | x::xs -> f x; print_string ", "; print_rem xs
  in print_rem list

let () =
  print_string "1.4: "; print_int @@ List.fold_left ( * ) 0 [6; 2; 1; 3] ;print_newline ();
  print_string "1.5: "; List.map (fun x -> [x :: []]) [(2, 8)] |> print_list_gen (print_list_gen (print_list_gen (fun (a,b) -> print_string "("; print_int a; print_string ","; print_int b; print_string ")"))); print_newline ();
  print_string "1.8: "; print_int ((fun a b -> a (a b)) (fun a -> a + 1) 7); print_newline ();
  print_string "Test trie \n";
  print_string "print cat, mouse and mousse\n";
  print_string "insert:\n" ;Trie.Node (false, []) |> Trie.insert "cat" |> Trie.insert "mouse" |> Trie.insert "mousse" |> print_trie;
  print_string "merge:\n" ;Trie.Node (false, []) |> Trie.insert "cat" |> Trie.insert "mouse" |> Trie.merge (Trie.Node (false, []) |> Trie.insert "mousse") |> print_trie;
  print_string "Test GraphSearch \n";
  let module GraphImplSearch = MakeGraphSearch (GraphImpl) in
  let a = GraphImpl.Node (3, []) in let b = GraphImpl.Node (2, [a]) in let c = GraphImpl.Node (4, [a]) in let d = GraphImpl.Node (1, [b; c]) in
  GraphImplSearch.dfs [a;b;c;d] d (fun () (Node (v, l)) -> print_int v) (); print_string " = 1234 \n";
  print_string "Test map_reduce\n";
  let l = [1.;0.5;0.2;0.4;0.7] in
  map_reduce (fun x -> delay x; x) (fun a b -> a+.b) l |> print_float; print_string " = 2.8\n";
