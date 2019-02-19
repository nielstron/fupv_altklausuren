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

let find1 s l = let rec helper s l =
                  match l with (sq, r)::xs -> if sq = s then ((Some r), xs) else 
                      let (fr, xs) = helper s xs in (fr, ((sq, r)::xs))
                             | [] -> (None, [])
  in let (res, xs) = helper s l in match res with None -> (None, xs)
                                                | Some r -> (res, (s,r)::xs)

module MFU = struct
  let rec init l = match l with (sq, r)::xs -> (0, (sq, r))::(init xs)
                              | [] -> []
  let find2 s l = let rec helper s l =
                    match l with 
                    | (f, (sq, r))::xs -> if sq = s then (Some (f+1, (sq, r)), xs) else 
                        let (res, xs) = helper s xs in
                        ( match res with None -> (None, (f, (sq, r))::xs)
                                       | Some (f2, (sq2, r2)) -> if f2 >= f then (res, (f, (sq, r))::xs) else
                                           (res, (f, (sq, r))::(f2, (sq2, r2))::xs))
                    | [] -> (None, [])
    in let (res, xs) = helper s l in match res with None -> (None, xs)
                                                  | Some (f, (s, r)) -> (Some r, xs)

end

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
      | x::xs -> A.show x ^ "; "
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
                                  | Some dc -> let _ = create (fun () -> sync (send b (Some (receive dc))))
                                    in serve v
    in serve []

  let publish broker key v = let ch = new_channel () in
    let rec publish () = sync (send ch v); publish ()
    in let _ = create publish ()
    in sync (send broker (Publish (key, ch)))
  let request broker key = let ch = new_channel () in
    sync (send broker (Request (key, ch))); match sync (receive ch) with
    | None -> None
    | Some repch -> sync (repch)
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
