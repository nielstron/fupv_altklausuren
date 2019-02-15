type trie = Node of bool * (char * trie) list

(* Source: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

(* 5.1. *)
let insert s tree = let cl = explode s in
  if cl = [] then let Node (_, children) = tree in Node (true, children)
  else
    let rec insert_into cl children =
      match children with 
      | [] -> (match cl with 
          | [] -> Invalid_argument "Some error" |> raise
          | [c] -> [(c, Node (true, []))] 
          | c::cs -> [(c, Node (false, insert_into cs []))])
      | (c, Node (ending, subchilds))::chs -> (match cl with 
          | [] -> Invalid_argument "Some error" |> raise
          | [cr] -> 
            if cr = c then 
              (c, Node (true, subchilds))::chs 
            else (c, Node (ending, subchilds))::(insert_into cl chs)
          | cr::crs -> 
            if cr = c then
              (c, Node (ending, (insert_into crs subchilds)))::chs
            else (c, Node (ending, subchilds))::(insert_into cl chs))
    in let Node (b, children) = tree in Node (b, insert_into cl children)

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
