open Thread
open Event

(*let _ = let rec f x = f x in f 1*)

let _ = fun f b a -> f (a,b) 

let _ = fun x -> fun x -> x, x 

let _ = fun x -> x (fun x -> x) 

(* 3. *)
module Peano = struct
  type nat = Z | S of nat
  let rec of_int i = if i <= 0 then Z else S (of_int (i-1))
  let to_int p = let rec help p r = match p with 
      | Z -> r
      | S n -> help n (r+1)
    in help p 0
  let rec add p1 p2 = match p1 with 
    | Z -> p2
    | S n -> add n (S p2)
  let rec mul p1 p2 = let rec helper p1 p2 r =
                        match p1 with
                        | Z -> r
                        | S n -> helper n p2 (add p2 r)
    in helper p1 p2 Z
  let rec sub p1 p2 = match p2 with
    | Z -> p1
    | S n2 -> match p1 with Z -> Z
                          | S n1 -> sub n1 n2
  let leq p1 p2 = if p1 = p2 then true else match sub p2 p1 with 
      | Z -> false
      | S _ -> true
end

(* 6.1 *)
let find1 s l = let rec helper s l =
                  match l with (sq, r)::xs -> if sq = s then ((Some r), xs) else 
                      let (fr, xs) = helper s xs in (fr, ((sq, r)::xs))
                             | [] -> (None, [])
  in let (res, xs) = helper s l in match res with None -> (None, xs)
                                                | Some r -> (res, (s,r)::xs)

(* 6.2 *)
let rec init l = match l with (sq, r)::xs -> (0, (sq, r))::(init xs)

                            | [] -> []

(* 6.3 *)
let find2 s l = let rec helper s l =
                  match l with 
                  | (f, (sq, r))::xs -> if sq = s then (false, Some (f+1, (sq, r)), xs) else 
                      let (ins, res, xs) = helper s xs in
                      if ins then (ins, res, (f, (sq, r))::xs) else
                        ( match res with None -> (true, None, (f, (sq, r))::xs)
                                       | Some (f2, (sq2, r2)) -> if f2 >= f then (false, res, (f, (sq, r))::xs) else
                                           (true, res, (f, (sq, r))::(f2, (sq2, r2))::xs))
                  | [] -> (true, None, [])
  in let (ins, res, xs) = helper s l in match res with None -> (None, xs)
                                                     | Some (f, (s, r)) -> if ins then (Some r, xs) else (Some r, (f, (s, r))::xs)

module type S = sig
  type t
  val size : t -> int
  val show : t -> string
end

module SPair (A:S) (B:S) = struct
  type t = A.t * B.t
  let size (a, b) = A.size a + B.size b
  let show (a, b) = "(" ^ A.show a ^ ", " ^ B.show b ^ ")"
end

module SList (A:S) = struct
  type t = A.t list
  let rec size a = match a with x::xs -> A.size x + size xs
                              | [] -> 0
  let show a = let rec helper a = match a with 
      | x::[] -> A.show x
      | x::xs -> A.show x ^ "; " ^ helper xs
      | [] -> ""
    in "[ " ^ helper a ^ "]"
end

module SEither (At:S) (Bt:S) = struct
  type t = A of At.t | B of Bt.t
  let rec size ab = match ab with A a -> At.size a
                                | B b -> Bt.size b
  let rec show ab = match ab with A a -> At.show a
                                | B b -> Bt.show b
end

module P2P = struct
  type ('a, 'b) t = Publish of 'a * 'b channel
                  | Request of 'a * 'b event option channel
  let broker () = let ch = new_channel () in
    let rec serve v = 
      match sync (receive ch) with 
      | Publish (a, b) -> serve ((a, b)::v)
      | Request (a, b) -> 
        let data_channel_opt = List.assoc_opt a v in
        match data_channel_opt with None -> 
          let _ = create (fun () -> sync (send b (None))) ()
          in serve v
                                  | Some dc -> let _ = create (fun () -> sync (send b (Some (receive dc)))) ()
                                    in serve v
    in let _ = create serve [] in ch

  let publish broker key v = let ch = new_channel () in
    let rec publish () = sync (send ch v); publish ()
    in let _ = create publish ()
    in sync (send broker (Publish (key, ch)))
  let request broker key = let ch = new_channel () in
    sync (send broker (Request (key, ch))); match sync (receive ch) with
    | None -> None
    | Some repch -> Some (sync (repch))
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
