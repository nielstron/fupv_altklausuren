module String = struct
    open String
    let to_list s = List.init (String.length s) (String.get s)
end

(*Aufgabe 3*)
let is_balanced arg = 
    let rec balance_help charList acc = 
        if acc < 0 then 
            false 
        else 
            (if (List.length (charList) >= 1) then
                (match List.hd charList with
                | '(' -> balance_help (List.tl charList) (acc + 1)
                | ')' -> balance_help (List.tl charList) (acc-1) 
                | _ -> balance_help (List.tl charList) acc
                )
            else
                acc = 0)

        in balance_help (String.to_list arg) 0

(*Aufgabe 6*)
type elem = H | N | O | Al | S
type bond = Atom of elem | Bond of (bond * int) list
type reaction = { reacts : (int * bond) list; prods : (int * bond) list }


let rec atoms el bon = 
    match bon with 
    | Atom(a) -> if a = el then 1 else 0
    | Bond(l) -> List.fold_left (fun akk (b, num) -> akk + ((atoms el b) *num)) 0 l

(*renaming *)
let is_balanced_reaction reac = 
    let rec giveAtoms b acc = 
        match b with [] -> acc
        | x::xs -> giveAtoms xs (match x with | Atom(e),i -> [e]
            | Bond(l),i -> giveAtoms l [])
    
    in let rec remove_dupl list = 
        List.fold_left (fun akk el -> if List.exists (fun e -> e = el) akk then akk else el::akk) [] list

    in let reverseLeft = List.map (fun (fst,snd) -> snd,fst) reac.reacts
    in let reverseRight = List.map (fun(fst,snd) -> snd,fst) reac.prods
    in let atleft = remove_dupl (giveAtoms reverseLeft [])
    in let atright = remove_dupl (giveAtoms reverseRight [])
    
    in let failure_list liste = List.filter (fun e-> atoms e (Bond(reverseLeft)) <> (atoms e (Bond(reverseRight)))) liste
    
    in if List.length (failure_list atleft) <> 0 || List.length (failure_list atright) <> 0 then false else true

(*Aufgabe 7*)
(*Vorsicht! Alle ab jetzt nicht getestet, da Module testen schwer*)
module type Iter = sig
type 'a t
type 'a s
val init : 'a t -> 'a s
val next : 'a s -> 'a option * 'a s
end

module ListIter = struct
    type 'a t = 'a list
    type 'a s = 'a list * int

    let init liste = liste, 0

    let next (l, index) =
    if List.length l <= index then
        None,(l,index)
    else
        Some (List.nth l index), (l, index+1)
end

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
(*other modules not implemented yet*)

module PairIter  (I1 : Iter) (I2 : Iter) = struct
    type ('a, 'b) t = 'a I1.t * 'b I2.t
    type ('a, 'b) s = 'a I1.s * 'b I2.s
    let init (it1, it2) = (I1.init it1, I2.init it2)
    let next (it1, it2) = 
        let (next1, newI1) = I1.next it1
    in  let (next2, newI2) = I2.next it2
    in  if next1 = None || next2 = None then
            ((None,None), (it1, it2))
        else
            ((next1, next2), (newI1, newI2))
end

module ExtIter (I : Iter) = struct  
    include I
    let next_filtered f iter = 
        let rec helper f1 iterator = 
            match I.next iterator with
            | (None, newI) -> None, iterator
            | (Some x, newI) ->
                if f x then (Some x, newI)
                else helper f newI
        in helper f iter

    let next_mapped f it =
        match I.next it with
        | (None, newI) -> (None, it)
        | (Some x, newI) -> (Some (f x), newI)
end  



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
    
    
                                                                                    

