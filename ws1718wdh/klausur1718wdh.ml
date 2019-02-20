open Thread
open Event

let todo () = Invalid_argument "TODO" |> raise


(* 3. *)
module Peano = struct
  type nat = Z | S of nat
  let of_int = todo
  let to_int = todo
  let add = todo
  let mul = todo
  let sub = todo
  let leq = todo
end

(* 6.1 *)
let find1 = todo

(* 6.2 *)
let init = todo

(* 6.3 *)
let find2 = todo

(* 7. *)
(* Module umbenannt nach S___ (zb SList) damit keine Clashes mit List stattfinden *)

module type S = sig
  type t
  val size : t -> int
  val show : t -> string
end

(* 8. *)

module P2P = struct
  type ('a, 'b) t = Publish of 'a * 'b channel
                  | Request of 'a * 'b event option channel
  let broker = todo

  let publish = todo

  let request = todo
end

(* ---- Test environment ---- *)

let p = Printf.printf
let () =  
  print_string "Test Peano\n";
  let module PeanoTest = struct
    open Peano
    let test () = 
      Printf.fprintf stdout "of_int: %d = %d\n" (to_int (of_int 3)) (to_int (S (S (S Z))));
      Printf.fprintf stdout "of_int: (neg): %d = %d\n" (to_int (of_int (-10))) (to_int Z);
      Printf.fprintf stdout "add: %d = %d\n" (to_int (add (of_int 5) (of_int 13))) 18;
      Printf.fprintf stdout "mul: %d = %d\n" (to_int (mul (of_int 3) (of_int 11))) 33;
      Printf.fprintf stdout "sub (leq): %d = %d\n" (to_int (sub (of_int 0) (of_int 11))) 0;
      Printf.fprintf stdout "sub (g): %d = %d\n" (to_int (sub (of_int 11) (of_int 3))) 8;
      Printf.fprintf stdout "sub (eq): %d = %d\n" (to_int (sub (of_int 11) (of_int 11))) 0;
      Printf.fprintf stdout "leq (leq): %b = %b\n" (leq (of_int 1) (of_int 3)) true;
      Printf.fprintf stdout "leq (g): %b = %b\n" (leq (of_int 11) (of_int 3)) false;
      Printf.fprintf stdout "leq (eq): %b = %b\n" (leq (of_int 3) (of_int 3)) true;
  end in PeanoTest.test ();
  print_string "Test find1\n";
  let module FindTest = struct
    let l = [("first", 1); ("second", 3); ("third", 4)]
    let print (r, l) = p "(";
      (match r with None -> p "None"
                  | Some s -> print_int s);
      p ", [";
      let rec helper l = 
        match l with (s, v)::xs -> Printf.printf "(%s, %d);" s v; helper xs
                   | [] -> ();
      in helper l;
      p "]\n"
    let test () = 
      p "Orig (requesting \"fourth\"): ";
      print @@ find1 "fourth" l;
      p "Requesting second: ";
      let (r, l) = find1 "second" l in
      print (r, l);
      p "Requesting third: ";
      let (r, l) = find1 "third" l in
      print (r, l)
  end in FindTest.test ();
  print_string "Test find2\n";
  let module FindTest = struct
    let l = [("first", 1); ("second", 3); ("third", 4)]
    let print (r, l) = p "(";
      (match r with None -> p "None"
                  | Some s -> print_int s);
      p ", [";
      let rec helper l = 
        match l with (h ,(s, v))::xs -> Printf.printf "(%d, (%s, %d));" h s v; helper xs
                   | [] -> ();
      in helper l;
      p "]\n"
    let test () = 
      p "Orig (requesting \"fourth\"): ";
      let (r, l) = find2 "fourth" @@ init l;
      in print (r, l);
      p "Requesting second: ";
      let (r, l) = find2 "second" l in
      print (r, l);
      p "Requesting third: ";
      let (r, l) = find2 "third" l in
      print (r, l);
      p "Requesting second: ";
      let (r, l) = find2 "second" l in
      print (r, l);
      p "Requesting first: ";
      let (r, l) = find2 "first" l in
      print (r, l)
  end in FindTest.test ();
  print_string "Test S\n";
  let module STest = struct
    module SFloat = struct
      type t = float
      let show a = string_of_float a
      let size a = int_of_float a
    end
    module SFloatPair = SPair (SFloat) (SFloat)
    module SFloatPairList = SList (SFloatPair)
    module SFloatPairListSFloatEither = SEither (SFloatPairList) (SFloat)
    let test () = 
      let v = SFloatPairListSFloatEither.A ([(1.1, 0.1); (2., 2.)]) in
      let w = SFloatPairListSFloatEither.B 0.5 in
      print_string @@ SFloatPairListSFloatEither.show v; print_string " =~ [(1.1, 0.1); (2., 2.)]\n";
      print_string @@ SFloatPairListSFloatEither.show w; print_string " = 0.5\n";
  end in STest.test ();
  print_string "Test P2P\n";
  let module P2PTest = struct
    open P2P
    let print a = match a with None -> print_string "None"
                             | Some v -> print_int v
    let test () = 
      let br = broker () in
      publish br "secretKey" 42;
      print (request br "anonymous"); print_string " = None\n";
      print (request br "secretKey"); print_string " = 42\n";
  end
  in P2PTest.test ()
