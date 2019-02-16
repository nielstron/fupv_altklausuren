open Thread
open Event

(* 1.6 *)
let onepoint6 = let f a b = a+a in f

(* 1.7 *)
(* let onepoint7 = (17, (fun i -> [2 * i - 7]) 3) :: (0, [6]) *)


(* 2.5. *)

let rec f a b c =
  if c = 0 then 1 else a * b * f a b (c-1)

let rec f a b c  = let rec f_end a b c r = if c = 0 then r else f_end a b (c-1) (a*b*r) in f_end a b c 1

(* 5. *)
module Trie = struct
  type trie = Node of bool * (char * trie) list

  (* Source: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

  (* 5.1. *)

  let insert s tree = let cl = explode s in
    if cl = [] then let Node (_, children) = tree in Node (true, children)
    else
      let rec insert_into cl children =
        match children with 
        | [] -> (match cl with 
            | [] -> Invalid_argument "Some error" |> raise
            | [c] -> [(c, Node (true, []))] 
            | c::cs -> [(c, Node (false, insert_into cs []))])
        | (c, Node (ending, subchilds))::chs -> (match cl with 
            | [] -> Invalid_argument "Some error" |> raise
            | [cr] -> 
              if cr = c then 
                (c, Node (true, subchilds))::chs 
              else (c, Node (ending, subchilds))::(insert_into cl chs)
            | cr::crs -> 
              if cr = c then
                (c, Node (ending, (insert_into crs subchilds)))::chs
              else (c, Node (ending, subchilds))::(insert_into cl chs))
      in let Node (b, children) = tree in Node (b, insert_into cl children)

  (* 5.2. *)

  let rec merge t1 t2 = let Node (b1, ch1) = t1 in let Node (b2, ch2) = t2 in
    let rec merge_child (char1, subtrie1) ch =
      match ch with
      | [] -> [(char1, subtrie1)]
      | x::xs -> let (char2, subtrie2) = x in
        if char1 = char2 then
          (char1, merge subtrie1 subtrie2)::xs
        else
          x::(merge_child (char1, subtrie1) xs)
    in
    let rec merge_children ch1 ch2 = 
      match ch1 with
      | [] -> ch2
      | x::xs -> merge_children xs (merge_child x ch2)
    in Node (b1 || b2, merge_children ch1 ch2)

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
  type node = Leaf of int | Node of node * int * node
  type graph = node

  let successors g n = match n with Leaf _ -> []
                                  | Node (l, _, r) -> [l, r]
end

(* 6.b *)
module GraphImpl = struct
  type node = Node of int * (node list)
  type graph = node list

  let successors g (Node (_,l)) = l
end

module MakeGraphSearch (G:Graph): sig
  val dfs : G.graph -> G.node -> ('a -> G.node -> 'a) -> 'a -> 'a 
end = struct
  let rec known n k = 
    match k with [] -> false
               | x::xs -> if n = x then true else known n xs
  let dfs g n f a =
    let rec dfs_rec g n f a k =
      if known n k then (a, k) else
        let k = n::k in
        let a = (f a n) in
        let s = G.successors g n in
        let rec dfs_rem_succ rem_suc f a k =
          match rem_suc with
          | [] -> (a,k)
          | x::xs -> let (a, k) = dfs_rec g x f a k
            in dfs_rem_succ xs f a k
        in dfs_rem_succ s f a k
    in let (a, k) = dfs_rec g n f a [] in a
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
  let rec pmap f l = match l with
    | [] -> [] 
    | x::xs ->   let ch = new_channel ()
      in 
      create (fun ch -> sync (send ch (f x))) ch;
      (receive ch)::(pmap f xs)

  (* 7.2. *)
  let rec preduce g a b = let ch = new_channel () in
    create (fun ch -> sync (send ch (g a b))) ch;
    receive ch

  (* 7.3. *)
  let rec remove_from_list a l r =
    match l with [] -> r
               | x::xs -> if a = x then xs@r else remove_from_list a xs (x::r)

  let rec reduce_list g l =
    print_string "List length: "; print_int (List.length l); print_newline();
    let rec better_list l before res =
      match l with [] -> res
                 | x::xs -> better_list xs (x::before) ((wrap x (fun r -> (r, before@xs)))::res )
    in let rec reduce_better_list v l =
         match l with [] -> let Some res = v in res
                    | x::xs -> let (res, rem) = select l in
                      match v with None -> reduce_better_list (Some res) (better_list rem [] [])
                                 | Some res2 -> reduce_better_list (None) (better_list ((preduce g res res2)::rem) [] [])
    in reduce_better_list None (better_list l [] [])


  (* 7.4. *)
  let map_reduce f g l = reduce_list g (pmap f l)

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
  ParallelReduce.map_reduce (fun x -> delay x; print_float x; print_newline(); x) (fun a b -> print_float (a+.b); a+.b) l |> print_float; print_string " = 2.8\n";
