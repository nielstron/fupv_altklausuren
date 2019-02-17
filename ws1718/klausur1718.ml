open Thread
open Event

module String = struct
  open String
  let to_list s = List.init (String.length s) (String.get s)
end

(*Aufgabe 3*)
let is_balanced arg = raise (Invalid_argument "TODO")

(*Aufgabe 6*)
type elem = H | N | O | Al | S
type bond = Atom of elem | Bond of (bond * int) list
type reaction = { reacts : (int * bond) list; prods : (int * bond) list }


let rec atoms el bon = raise (Invalid_argument "TODO")




(*Umbenannt von is_balanced da in Aufgabe 3 selber Name*)
let is_balanced_reaction reac = raise (Invalid_argument "TODO")



(*Aufgabe 7*)
module type Iter = sig
  type 'a t
  type 'a s
  val init : 'a t -> 'a s
  val next : 'a s -> 'a option * 'a s
end

module ListIter = struct
end

module TreeIter = struct
end

module ExtIter (I:Iter) = struct
end

module PairIter (F:Iter) (G:Iter) = struct
end

(* Aufgabe 8 *)
module Blog = struct
end


(*... *)

(*tests*)

module Iterate (I:Iter) = struct
  let to_list a = 
    let rec iter_rec s = match I.next s with (Some x, ns) -> x::(iter_rec ns)
                                           | (None, _) -> [] 
    in iter_rec (I.init a)
end
module ListIterate = Iterate (ListIter)
module TreeIterate = Iterate (TreeIter)
module ExtListIterate = ExtIter (ListIter)
module TreeListIter = PairIter (ListIter) (TreeIter)

let test expected result = 
  if expected = result then 
    Printf.printf "Test correct\n"
  else
    Printf.printf "Test failed\n"

let testList exp res =
  if List.length exp = List.length res then
    let rec helper e r = 
      match e with [] -> Printf.printf "finished.\n"
                 | x::xs -> (match r with [] -> raise (Invalid_argument "failure")
                                        | x'::xs' -> (test x x'; helper xs xs'))
    in helper exp res            
  else
    Printf.printf "testList failed\n"

let ammoniumsulfat = Bond(
    [Bond([Atom(N), 1; Atom(H), 4]), 2;
     Atom(S), 1;
     Atom(O), 4
    ])

let sauerstoff = Bond([Atom(O), 2])
let wasserstoff = Bond([Atom(H), 2])
let wasser = Bond([Atom(H), 2; Atom(O), 1])
let wasserGleichung = {reacts = [1, sauerstoff; 2, wasserstoff]; prods = [2, wasser]}
let al2o3 = Bond([Atom(Al), 2; Atom(O), 3])

let falschealogleichung = {reacts = [1, Atom(Al); 1, sauerstoff]; prods = [1, al2o3]}
let richtigealogleichung = {reacts = [4, Atom(Al); 3, sauerstoff]; prods = [2, al2o3]}

let main = Printf.printf "Tests Aufgabe 3:\n";
  testList [false; false; true; true] [is_balanced ")("; is_balanced "(a)b)"; is_balanced "foo"; is_balanced "a(b(c)d)e"];
  Printf.printf "Tests Aufgabe 6.1:\n";
  testList [0; 2; 1; 8; 2; 0] [atoms H (Bond([Atom(O), 2])); atoms O (Bond([Atom(O), 2])); atoms O (Bond([Atom(H), 2; Atom(O), 1])); 
                               atoms H ammoniumsulfat; atoms N ammoniumsulfat; atoms Al ammoniumsulfat];
  Printf.printf "Tests Aufgabe 6.2:\n";
  testList [true; false; true] [is_balanced_reaction wasserGleichung; is_balanced_reaction falschealogleichung; is_balanced_reaction richtigealogleichung];
  print_string "Tests Aufgabe 7:\n";
  print_string "ListIterate:\n";
  let l = [1;2;3;4;5] in
  testList l (ListIterate.to_list l);
  print_string "TreeIterate:\n";
  let t = TreeIter.Node (4, TreeIter.Node (2, TreeIter.Leaf 1, TreeIter.Leaf 3), TreeIter.Node (6, TreeIter.Leaf 5, TreeIter.Leaf 7))
  in testList [1;2;3;4;5;6;7] (TreeIterate.to_list t);
  print_string "PairIterate:\n";
  let (av, astate) = TreeListIter.next(TreeListIter.init (l, t)) in 
  let (b, _) = TreeListIter.next (let (_, temp) = TreeListIter.next (let (_, temp) = TreeListIter.next ( let (_, temp) = TreeListIter.next ( let (_, temp) = TreeListIter.next (astate) in temp ) in temp ) in temp) in temp) in
  testList [Some (1,1); None] [av;b];
  print_string "ExtIter: \n";
  let (shouldbethree, temp) = ExtListIterate.next_filtered (fun x -> x=3) (ListIter.init [2;3]) in 
  let (shouldbenone, temp) = ExtListIterate.next_filtered (fun x -> x=3) (temp) in 
  testList [Some 3; None] [shouldbethree; shouldbenone];
  let (shouldbethree, temp) = ExtListIterate.next_mapped (fun x -> x*.2.) (ListIter.init [1.5]) in 
  let (shouldbenone, temp) = ExtListIterate.next_mapped (fun x -> x*.2.) (temp) in 
  testList [Some 3.; None] [shouldbethree; shouldbenone];
  print_string "Tests Aufgabe 8 (Blog):\n";
  let s = Blog.start_server [("userA", "passA"); ("userB", "passB")] in
  Blog.post s "userB" "passB" "Welcome to my OCaml blog.";
  Blog.post s "userA" "passA" "My name is A and I’m starting my own blog!";
  Blog.post s "userB" "12345" "I am a hacker attacking B’s blog now!";
  testList [] (Blog.read s "anonymous");
  Blog.post s "userB" "passB" "You can have threads in OCaml!";
  testList ["Welcome to my OCaml blog."; "You can have threads in OCaml!"] (Blog.read s "userB");






