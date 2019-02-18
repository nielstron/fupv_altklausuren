open Thread
open Event

(* 3. *)

module CompressedTree = struct
  type 'a t = Leaf | Node of 'a * 'a t * 'a t
  type 'a c = CLeaf | Uneq of 'a * 'a c * 'a c | Eq of 'a * 'a c

  let rec compress t = 
    match t with 
    | Leaf -> CLeaf 
    | Node (v, l, r) -> if l = r then Eq (v, compress l) else Uneq (v, compress l, compress r)

  let rec count t = 
    match t with 
    | CLeaf -> 0
    | Eq (v, c) -> 1 + 2*(count c)
    | Uneq (v, l, r) -> 1 + (count l) + (count r)

  let rec merge f a b = 
    match a with 
    | CLeaf -> b 
    | Eq (va, ca) -> (match b with 
        | CLeaf -> a
        | Eq (vb, cb) -> Eq (f va vb, merge f ca cb)
        | Uneq (vb, lb, rb) -> let lnew = merge f ca lb in let rnew = merge f ca rb in
          if lnew = rnew then Eq (f va vb, lnew) else Uneq (f va vb, lnew, rnew))
    | Uneq (va, la, ra) -> (match b with
        | CLeaf -> a
        | Eq (vb, cb) -> let lnew = merge f la cb in let rnew = merge f ra cb in
          if lnew = rnew then Eq (f va vb, lnew) else Uneq (f va vb, lnew, rnew)
        | Uneq (vb, lb, rb) -> let lnew = merge f la lb in let rnew = merge f ra rb in
          if lnew = rnew then Eq (f va vb, lnew) else Uneq (f va vb, lnew, rnew)
      )
end

(* 4. *)
module Server : sig
  type ('a, 'b) t
  val serve : ('a -> 'b) -> ('a, 'b) t
  val request : ('a, 'b) t -> 'a -> 'b
end = struct
  type ('a, 'b) t = ('a option * 'b channel * 'b option) channel
  let serve f = let ch = new_channel () in
    let rec serve_func rep_t_list = 
      match select ((receive ch)::rep_t_list) with
      | (Some ra, rep, None) -> 
        let tch = new_channel () in
        let t_func () = sync (send tch (None, rep, Some (f ra))) in
        create t_func (); serve_func ((receive tch)::rep_t_list)
      | (None, rep, Some vb) -> sync (send rep vb); serve_func rep_t_list
      | _ -> Invalid_argument "erroneous request" |> raise
    in create serve_func []; ch
  let request server va = let ch = new_channel () in
    sync (send server (Some va, ch, None)); sync (receive ch)
end

(* 6. *)
module type Hashable = sig
  type t
  val hash : t -> int
end


(* --- Test environment --*)

let () =
  print_string "Test server\n";
  let s = Server.serve (fun (x, s) -> delay x; print_string s; print_newline ()) in
  let t1 = create (fun _ -> Server.request s (0.5, "I was requested first but should be printed second")) () in 
  let t2 = create (fun _ -> delay 0.1; Server.request s (0.1, "I was requested second but should be printed first")) () in
  join t1; join t2;