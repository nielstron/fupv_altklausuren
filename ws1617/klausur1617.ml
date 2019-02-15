type trie = Node of bool * (char * trie) list

(* Source: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

(* 5.1. *)

(* --- Test environment ---*)

let print_char_list list = 
  print_string "[";
  let rec print_rem list = 
    match list with [] -> print_string "]"; print_newline ()
                  | x::xs -> print_char x; print_rem xs
  in print_rem list

let print_trie tree = let rec iter_words tree cur = 
                        let Node (b, children) = tree in
                        (if b then print_char_list cur;
                         let rec iter_children childs cur = 
                           match childs with 
                           | [] -> ()
                           | (c, sub)::xs -> iter_words sub (cur@[c]); iter_children xs cur
                         in iter_children children cur)
  in iter_words tree []

let () =
  print_string "Test trie insert \n";
  print_string "print cat, mouse and mousse\n";
  Node (false, []) |> insert "cat" |> insert "mouse" |> insert "mousse" |> print_trie;
