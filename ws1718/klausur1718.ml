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
    Printf.printf "testList failed"

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
  testList [true; false; true] [is_balanced_reaction wasserGleichung; is_balanced_reaction falschealogleichung; is_balanced_reaction richtigealogleichung]




