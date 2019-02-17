open Thread
open Event

module String = struct
  open String
  let to_list s = List.init (String.length s) (String.get s)
end

(*Aufgabe 3*)
let is_balanced arg = let rec balanced_list until_now rem =
                        match rem with [] -> until_now = 0
                                     | x::xs -> match x with '(' -> balanced_list (until_now+1) xs
                                                           | ')' -> if until_now > 0 then balanced_list (until_now-1) xs else false
                                                           | _ -> balanced_list until_now xs
  in balanced_list 0 (String.to_list arg)

(*Aufgabe 6*)
type elem = H | N | O | Al | S
type bond = Atom of elem | Bond of (bond * int) list
type reaction = { reacts : (int * bond) list; prods : (int * bond) list }


let rec atoms el bon = match bon with
  | Atom elem -> if el = elem then 1 else 0
  | Bond remlist -> match remlist with 
    | [] -> 0
    | (b, c)::xs -> (atoms el b) * c + (atoms el (Bond xs))



(*Umbenannt von is_balanced da in Aufgabe 3 selber Name*)
let is_balanced_reaction reac = 
  let rec atoms_on_side el side = match side with 
    | (c, b)::xs -> (atoms el b) * c + (atoms_on_side el xs)
    | [] -> 0
  in match reac with { reacts= left; prods= right} -> 
    let rec atoms_in_bond b = match b with Atom e -> [e] | Bond ((x, _)::xs) -> (atoms_in_bond x)@(atoms_in_bond (Bond xs)) | Bond [] -> []
    in
    let rec atoms_in_side side =
      match side with [] -> []
                    | (c, b)::xs -> (atoms_in_bond b)@(atoms_in_side xs)
    in
    let rec check_side side others =
      let a = atoms_in_side side in let rec check_rem_equal a others =
                                      match a with [] -> true
                                                 | x::xs -> if (atoms_on_side x side) = (atoms_on_side x others) then check_rem_equal xs others else false
      in check_rem_equal a others
    in (check_side left right) && (check_side right left)


(*Aufgabe 7*)
module type Iter = sig
  type 'a t
  type 'a s
  val init : 'a t -> 'a s
  val next : 'a s -> 'a option * 'a s
end

module ListIter = struct
  type 'a t = 'a list
  type 'a s = 'a list
  let init l = l
  let next s = match s with [] -> (None, []) | x::xs -> (Some x, xs)
end

(* Learn this once and never forget *)
(* https://www.geeksforgeeks.org/inorder-tree-traversal-without-recursion/ *)
module TreeIter = struct
  type 'a t = Leaf of 'a | Node of 'a * 'a t * 'a t
  type 'a s = 'a t list
  let rec traverse_left tree par =
    match tree with Leaf x -> tree::par
                  | Node (x, l, r) -> traverse_left l (tree::par)
  let init tree = traverse_left tree []
  let next state = match state with [] -> (None, [])
                                  | n::par -> match n with Leaf x -> (Some x, par)
                                                         | Node (x, l, r) -> (Some x, traverse_left r par)
end

module ExtIter (I:Iter) = struct
  let rec next_filtered p s = match I.next s with
    | (None, x) -> (None, x)
    | (Some v, x) -> if p v then (Some v, x) else next_filtered p x
  let rec next_mapped m s = match I.next s with
    | (None, x) -> (None, x)
    | (Some v, x) -> (Some (m v), x)
end

module PairIter (F:Iter) (G:Iter) = struct
  type ('a, 'b) t = ('a F.t) * ('b G.t)
  type ('a, 'b) s = ('a F.s) * ('b G.s)
  let init x = let (f, g) = x in (F.init f, G.init g)
  let next (f, g) = match F.next f with (None, f) -> (None, (f, g))
                                      | (Some x, nf) -> match G.next g with (None, g) -> (None, (f, g))
                                                                          | (Some y, ng) -> (Some (x,y), (nf, ng))
end

(* Aufgabe 8 *)
module Blog = struct
  type blog = string list
  type user = string
  type pass = string
  type message = Post of user * pass * string
               | Read of user * blog channel
  type t = message channel
  let start_server (ulist: ((user*pass) list)) = let ch = new_channel () in
    let rec get_user_pw u = List.assoc_opt u ulist in 
    let rec user_blog u blogs res = match blogs with [] -> res
                                                   | (x, b)::blogs -> if u = x then user_blog u blogs (b::res) else user_blog u blogs res
    in
    let rec server_thread blogs = let m = sync (receive ch) in
      match m with Post (u, p, b) -> (match get_user_pw u with None -> server_thread blogs
                                                             | Some x -> if x = p then server_thread ((u, b)::blogs) else server_thread blogs)
                 | Read (u, rep) -> sync (send rep (user_blog u blogs [])); server_thread blogs
    in let _ = create server_thread [] in (); ch

  let post server user pass text = sync (send server (Post (user, pass, text)))
  let read server user = let r = new_channel () in sync (send server (Read (user,r ))); sync (receive r)
end



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






