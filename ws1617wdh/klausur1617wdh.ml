open Thread
open Event

type 'a anytree = Any of 'a anytree list
                | BinOp of ('a -> 'a -> 'a) * 'a anytree * 'a anytree
                | Const of 'a

(* 5 *)


module type Machine = sig
  type instr
  type state
  val start : int -> state
  val exec : state -> instr -> state
  val get_result : state -> int
end

(* 6.1 *)

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

end

(* 6.3 *)


type btree = Node of (int * btree) list
           | Empty
type split_res = Split of btree * int * btree
               | Plain of btree

(* 7 a) *)

(* 7 b) *)


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
  print_int (StackEvaluator.run [StackEvaluator.Ite ([StackEvaluator.Instr (StackMachine.Const 2)],[StackEvaluator.Instr (StackMachine.Const 10)])] 0); print_string " = 10\n";
  print_int (StackEvaluator.run [StackEvaluator.Ite ([StackEvaluator.Instr (StackMachine.Const 2)],[StackEvaluator.Instr (StackMachine.Const 10)])] 3); print_string " = 2\n";
  print_string "Test Register-Machine\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MoveReg (RegisterMachine.Ra,RegisterMachine.Rc)))); print_string " = 1\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MovConst (RegisterMachine.Ra,5)))); print_string " = 5\n";
  print_int (RegisterMachine.get_result (RegisterMachine.exec (RegisterMachine.exec (RegisterMachine.start 1) (RegisterMachine.MovConst (RegisterMachine.Ra,5))) (RegisterMachine.Add (RegisterMachine.Ra, RegisterMachine.Rc)))); print_string " = 6\n";
  print_string "Test eval\n";
  let tree = Any [BinOp ((fun a b -> (delay 1.); a),(Const 1),(Const 5)); Const 2] in print_int @@ eval tree; print_string " = 2\n";
  let tree = Any [Const 2; BinOp ((fun a b -> (delay 1.); a),(Const 1),(Const 5))] in print_int @@ eval tree; print_string " = 2\n";