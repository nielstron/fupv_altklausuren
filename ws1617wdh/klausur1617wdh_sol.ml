open Thread
open Event

type 'a anytree = Any of 'a anytree list
                | BinOp of ('a -> 'a -> 'a) * 'a anytree * 'a anytree
                | Const of 'a

(* 5 *)
let rec eval tree = match tree with
    Const a -> a 
  | BinOp (f,l,r) -> f (eval l) (eval r)
  | Any l -> let rep = new_channel () in 
    let rec start_threads l = (match l with 
          x::xs -> 
          (let evaluate (req, reply) = 
             let tree = sync(receive req) in sync (send reply (eval tree))
           in let req = new_channel ()
           in let _ = create evaluate (req, rep) in sync(send req x)); start_threads xs
        | [] -> sync (receive rep))
    in start_threads l


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
                   | Ite (t, e) -> if M.get_result s != 0 then execute s t else execute s e
      ) |
      [] -> s
  let run prog input = let state = M.start input in let res = execute state prog in M.get_result res
end


type btree = Node of (int * btree) list
           | Empty
type split_res = Split of btree * int * btree
               | Plain of btree

(* 7 a) *)
let split b node = match node with Empty -> Plain node 
                                 | Node node_list -> 
                                   if List.length node_list <= b then Plain node
                                   else 
                                     let rec split_node rem i = 
                                       if i = 0 then 
                                         match rem with 
                                         |[] -> Plain node
                                         | x::xs -> let (value, subtree) = x in Split (Node [(max_int, subtree)], value, Node xs)
                                       else match rem with
                                         | [] -> Plain node
                                         | x::xs -> let Split (Node l, value, r) = split_node xs (i-1) in Split (Node (x::l), value, r)
                                     in split_node  node_list (List.length node_list /2)

(* 7 b) *)
let insert_into_leaf b key tree = match tree with 
  | Empty -> Plain (Node [(key, Empty); (max_int, Empty)])
  | Node node_list -> let rec insert_into_list b (key, subtree) list = match list with
      | [] -> [(key, subtree)]
      | x::xs -> let (upper_bound, _) = x in
        if upper_bound > key then (key, subtree)::x::xs else x::insert_into_list b (key, subtree) xs
    in let res_node = Node (insert_into_list b (key, Empty) node_list) in
    split b res_node

let rec insert_into_node b key tree = 
  match tree with
  | Empty -> insert_into_leaf b key tree 
  | Node node_list -> match node_list with 
    | [] -> raise @@ Invalid_argument "error"
    | (_, Empty)::xs -> insert_into_leaf b key tree
    | xs ->  let rec insert_into_list b key list = match list with 
        | [] -> raise @@ Invalid_argument "error"
        | (upper_bound, subtree)::xs -> if key >= upper_bound then (upper_bound, subtree)::(insert_into_list b key xs)
          else match insert_into_node b key subtree with 
            | Plain subtree -> (upper_bound, subtree)::xs
            | Split (l, s, r) -> (s, l)::(upper_bound, r)::xs
      in split b @@ Node (insert_into_list b key node_list)

let insert b key tree = match insert_into_node b key tree with
  | Plain tree -> tree
  | Split (l, s, r) -> Node [(s, l);(max_int, r)]


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
  print_string "All following lists should be  in order \n";
  print_list (insert_values 4 [1; 3; 10] Empty |> in_order_tree );
  print_list (insert_values 4 [2; 1; 10] Empty |> in_order_tree );
  print_list (insert_values 4 [2; 1; 10; 5; 20; 13] Empty |> in_order_tree );
  print_string "Test StackMachine und Machine-Evaluator\n";
  let module StackEvaluator = MakeProgramEvaluator(StackMachine) in
  print_int (StackEvaluator.run [StackEvaluator.Instr (StackMachine.Const 2); StackEvaluator.Instr StackMachine.Add] 3); print_string " = 5\n";
  print_int (StackEvaluator.run [StackEvaluator.Instr (StackMachine.Const 2); StackEvaluator.Instr (StackMachine.Scale2 10)] 3); print_string " = 20\n";
  print_int (StackEvaluator.run [StackEvaluator.Ite ([StackEvaluator.Instr (StackMachine.Const 2)],[StackEvaluator.Instr (StackMachine.Const 10)])] 0); print_string " = 10\n";
  print_int (StackEvaluator.run [StackEvaluator.Ite ([StackEvaluator.Instr (StackMachine.Const 2)],[StackEvaluator.Instr (StackMachine.Const 10)])] 3); print_string " = 2\n";
  print_string "Test Register-Machine\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MoveReg (RegisterMachine.Ra,RegisterMachine.Rc)))); print_string " = 1\n";