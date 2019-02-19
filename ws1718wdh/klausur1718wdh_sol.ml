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
  let rec mul p1 p2 = match p1 with
    | Z -> Z
    | S n -> add p2 (mul n p2)
  let rec sub p1 p2 = match p2 with
    | Z -> p1
    | S n2 -> match p1 with Z -> Z
                          | S n1 -> sub n1 n2
  let leq p1 p2 = if p1 = p2 then true else match sub p2 p1 with 
      | Z -> false
      | S _ -> true
end

(* ---- Test environment ---- *)

open Peano
let () =  
  Printf.fprintf stdout "%d = %d\n" (to_int (of_int 3)) (to_int (S (S (S Z))));
  Printf.fprintf stdout "%d = %d\n" (to_int (of_int (-10))) (to_int Z);
  Printf.fprintf stdout "%d = %d\n" (to_int (add (of_int 5) (of_int 13))) 18;
  Printf.fprintf stdout "%d = %d\n" (to_int (mul (of_int 3) (of_int 11))) 33;
  Printf.fprintf stdout "%d = %d\n" (to_int (sub (of_int 0) (of_int 11))) 0;
  Printf.fprintf stdout "%d = %d\n" (to_int (sub (of_int 11) (of_int 0))) 11;
  Printf.fprintf stdout "%d = %d\n" (to_int (sub (of_int 11) (of_int 3))) 8;
  Printf.fprintf stdout "%b = %b\n" (leq (of_int 11) (of_int 3)) false;
  Printf.fprintf stdout "%b = %b\n" (leq (of_int 3) (of_int 3)) true;
  Printf.fprintf stdout "%b = %b\n" (leq (of_int 1) (of_int 3)) true;
