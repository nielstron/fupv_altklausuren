open Thread
open Event

type 'a anytree = Any of 'a anytree list
                | BinOp of ('a -> 'a -> 'a) * 'a anytree * 'a anytree
                | Const of 'a

(* 5 *)
let rec eval = function
  | Const a -> a
  | BinOp (op,a,b) ->
    let ca,cb = new_channel (), new_channel () in
    let _,_ = create (fun _ -> sync @@ send ca (eval a)) (),
              create (fun _ -> sync @@ send cb (eval b)) () in
    op (sync @@ receive ca) (sync @@ receive cb)
  | Any l -> select (l |> List.map (fun a ->
      let c = new_channel () in
      let _ = create (fun _ -> sync @@ send c (eval a)) () in
      receive c
    ))


module type Machine = sig
  type instr
  type state
  val start : int -> state
  val exec : state -> instr -> state
  val get_result : state -> int
end

(* 6.1 *)
module StackMachine = struct
  type instr = Const of int | Add | Scale2 of int
  type state = int list
  let start init = [init]
  let get_result state = match state with x::xs -> x | [] -> 0
  let exec s i = match i with Const value -> value::s 
                            | Add -> (match s with x::xs -> match xs with y::ys -> (x+y)::ys)
                            | Scale2 fac -> (match s with x::xs -> match xs with y::ys -> (x*fac)::((y*fac)::ys))
end

(* 6.2 *)
module RegisterMachine = struct
  type reg = Ra | Rb | Rc
  type rm_state = { ra : int; rb : int; rc : int }
  let set (r : reg) (v : int) (s : rm_state) : rm_state =
    match r with
      Ra -> { s with ra = v }
    | Rb -> { s with rb = v }
    | Rc -> { s with rc = v }
  let get (r : reg) (s : rm_state) : int =
    match r with
      Ra -> s.ra
    | Rb -> s.rb
    | Rc -> s.rc

  type instr = MoveReg of reg*reg | MovConst of reg*int | Add of reg*reg
  let start init = {ra= 0; rb=0; rc= init}
  let get_result state = state.ra
  let exec s i = match i with MoveReg (r1, r2) -> set r1 (get r2 s) s
                            | MovConst (r1, value) -> set r1 value s
                            | Add (r1, r2) -> set r1 ((get r1 s) + (get r2 s) ) s
end

(* 6.3 *)
module MakeProgramEvaluator (M:Machine) = struct
  type statement = Instr of M.instr
                 | Ite of statement list * statement list
  type state = M.state
  let rec execute s stmts = match stmts with 
      x::xs -> (
        match x with Instr i -> execute (M.exec s i) xs
                   | Ite (t, e) -> if M.get_result s != 0 then execute s (t @ xs) else execute s (e @ xs)
      ) |
      [] -> s
  let run prog input = let state = M.start input in let res = execute state prog in M.get_result res
end


open List

type btree =
  | Node of (int * btree) list
  | Empty

type split_res =
  | Split of btree * int * btree
  | Plain of btree

(* 7 a) *)
let split b c =
  let rec aux l i = function [] -> failwith "split []"
                           | x::xs when i > 0 -> aux (x::l) (i-1) xs
                           | (x,t)::xs -> (rev ((max_int,t)::l)), x, xs in
  if (length c) <= b then Plain (Node c) else
    let l,x,r = aux [] (b / 2) c in Split ((Node l),x,(Node r))

(* 7 b) *)
let insert b k t =
  let rec aux = function Empty -> Split (Empty,k,Empty)
                       | Node(l) -> split b (list_ins [] l)
  and list_ins acc = function [] -> failwith "list_ins []"
                            | (x,n)::xs when k < x -> (match (aux n) with
                                | Plain t' -> rev_append acc ((x,t')::xs)
                                | Split (t1,x',t2) -> rev_append ((x',t1)::acc) ((x,t2)::xs))
                            | (x,n)::xs -> list_ins ((x,n)::acc) xs in
  match (aux t) with Plain t' -> t' | Split (t1,x,t2) -> Node [x,t1; max_int,t2]

(* ------ Testing Environment -------- *)

let rec in_order_tree tree = match tree with 
  | Empty -> []
  | Node node_list -> let rec in_order_list list = 
                        match list with
                        | [] -> []
                        | (key, subtree)::xs -> in_order_tree subtree @ key::in_order_list xs 
    in in_order_list node_list

let rec insert_values b values tree =
  match values with [] -> tree
                  | x::xs -> insert_values b xs (insert b x tree)

let print_list list = 
  print_string "[ ";
  let rec print_rem list = 
    match list with [] -> print_string "]"; print_newline ()
                  | x::xs -> print_int x; print_string ", "; print_rem xs
  in print_rem list

let () = 
  print_string "Test b-Trees\n";
  print_string "All following lists should be  in order except for max_int (not an error! this marks ends of nodes) \n";
  print_list (insert_values 4 [1; 3; 10] Empty |> in_order_tree );
  print_list (insert_values 4 [2; 1; 10] Empty |> in_order_tree );
  print_list (insert_values 4 [2; 1; 10; 5; 20; 13] Empty |> in_order_tree );
  print_string "Test StackMachine und Machine-Evaluator\n";
  let module StackEvaluator = MakeProgramEvaluator(StackMachine) in
  print_int (StackEvaluator.run [StackEvaluator.Instr (StackMachine.Const 2); StackEvaluator.Instr StackMachine.Add] 3); print_string " = 5\n";
  print_int (StackEvaluator.run [StackEvaluator.Instr (StackMachine.Const 2); StackEvaluator.Instr (StackMachine.Scale2 10)] 3); print_string " = 20\n";
  print_int (StackEvaluator.run [StackEvaluator.Ite ([StackEvaluator.Instr (StackMachine.Const 2)],[StackEvaluator.Instr (StackMachine.Const 10)]); StackEvaluator.Instr StackMachine.Add] 0); print_string " = 10\n";
  print_int (StackEvaluator.run [StackEvaluator.Ite ([StackEvaluator.Instr (StackMachine.Const 2)],[StackEvaluator.Instr (StackMachine.Const 10)]); StackEvaluator.Instr StackMachine.Add] 3); print_string " = 5\n";
  print_string "Test Register-Machine\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MoveReg (RegisterMachine.Ra,RegisterMachine.Rc)))); print_string " = 1\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MovConst (RegisterMachine.Ra,5)))); print_string " = 5\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MovConst (RegisterMachine.Ra,5))) (RegisterMachine.Add (RegisterMachine.Ra, RegisterMachine.Rc)))); print_string " = 6\n";
  print_string "Test eval\n";
  let tree = Any [BinOp ((fun a b -> (delay 1.); a),(Const 1),(Const 5)); Const 2] in print_int @@ eval tree; print_string " = 2\n";
  let tree = Any [Const 2; BinOp ((fun a b -> (delay 1.); a),(Const 1),(Const 5))] in print_int @@ eval tree; print_string " = 2\n";
